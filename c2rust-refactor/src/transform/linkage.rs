use std::collections::HashMap;
use rustc_hir::def_id::DefId;
use rustc_ast::*;
use rustc_ast::ptr::P;
use rustc_span::symbol::Symbol;
use rustc_span::sym;
use smallvec::smallvec;

use crate::ast_manip::{FlatMapNodes, MutVisitNodes, visit_nodes};
use crate::ast_manip::fn_edit::{visit_fns, FnKind};
use crate::command::{CommandState, Registry};
use crate::driver::{Phase};
use crate::path_edit::fold_resolved_paths;
use crate::transform::Transform;
use crate::RefactorCtxt;


/// # `link_funcs` Command
/// 
/// Usage: `link_funcs`
/// 
/// Link up function declarations and definitions with matching symbols across
/// modules.  For every foreign `fn` whose symbol matches a `fn` definition
/// elsewhere in the program, it replaces all uses of the foreign `fn` with a
/// direct call of the `fn` definition, and deletes the foreign `fn`.
/// 
/// Example:
/// 
/// ```ignore
///     mod a {
///         #[no_mangle]
///         unsafe extern "C" fn foo() { ... }
///     }
/// 
///     mod b {
///         extern "C" {
///             // This resolves to `a::foo` during linking.
///             fn foo();
///         }
/// 
///         unsafe fn use_foo() {
///             foo();
///         }
///     }
/// ```
/// 
/// After running `link_funcs`:
/// 
/// ```ignore
///     mod a {
///         #[no_mangle]
///         unsafe extern "C" fn foo() { ... }
///     }
/// 
///     mod b {
///         // 1. Foreign fn `foo` has been deleted
///         unsafe fn use_foo() {
///             // 2. `use_foo` now calls `foo` directly
///             ::a::foo();
///         }
///     }
/// ```
pub struct LinkFuncs;

impl Transform for LinkFuncs {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        // (1) Find all `#[no_mangle]` or `#[export_name=...]` functions, and index them by symbol.
        // (2) Find all extern fns, and index them by def_id.
        let mut symbol_to_def = HashMap::new();
        let mut extern_def_to_symbol = HashMap::new();

        visit_fns(krate, |fl| {
            let def_id = cx.node_def_id(fl.id);
            if fl.kind != FnKind::Foreign {
                if let Some(name) = crate::util::first_attr_value_str_by_name(&fl.attrs, sym::export_name) {
                    symbol_to_def.insert(name, def_id);
                } else if crate::util::contains_name(&fl.attrs, sym::no_mangle) {
                    symbol_to_def.insert(fl.ident.name, def_id);
                }
            } else {
                extern_def_to_symbol.insert(def_id, fl.ident.name);
            }
        });

        // (3) Adjust references to extern fns to refer to the `#[no_mangle]` definition instead.
        fold_resolved_paths(krate, cx, |qself, path, def| {
            if let Some(def_id) = def[0].opt_def_id() {
                if let Some(&symbol) = extern_def_to_symbol.get(&def_id) {
                    if let Some(&real_def_id) = symbol_to_def.get(&symbol) {
                        return (None, cx.def_path(real_def_id));
                    }
                }
            }
            (qself, path)
        });

        // (4) Remove unused externs
        MutVisitNodes::visit(krate, |fm: &mut ForeignMod| {
            fm.items.retain(|i| {
                let def_id = cx.node_def_id(i.id);
                // Drop any items that resolve to a symbol in another module.
                if let Some(&symbol) = extern_def_to_symbol.get(&def_id) {
                    if let Some(&_real_def_id) = symbol_to_def.get(&symbol) {
                        return false;
                    }
                }
                true
            });
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `link_incomplete_types` Command
/// 
/// Usage: `link_incomplete_types`
/// 
/// Link up type declarations and definitions with matching names across modules.
/// For every foreign type whose name matches a type definition elsewhere in the
/// program, it replaces all uses of the foreign type with the type definition, and
/// deletes the foreign type.
/// 
/// Example:
/// 
/// ```ignore
///     mod a {
///         struct Foo { ... }
///     }
/// 
///     mod b {
///         extern "C" {
///             type Foo;
///         }
/// 
///         unsafe fn use_foo(x: &Foo) { ... }
///     }
/// ```
/// 
/// After running `link_incomplete_types`:
/// 
/// ```ignore
///     mod a {
///         struct Foo { ... }
///     }
/// 
///     mod b {
///         // 1. Foreign fn `Foo` has been deleted
///         // 2. `use_foo` now references `Foo` directly
///         unsafe fn use_foo(x: &::a::Foo) { ... }
///     }
/// ```
pub struct LinkIncompleteTypes;

impl Transform for LinkIncompleteTypes {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        // (1) Find complete type definitions, and index them by name.
        let mut name_to_complete = HashMap::new();
        let mut incomplete_to_name = HashMap::new();

        visit_nodes(krate, |i: &Item| {
            let complete = match i.kind {
                ItemKind::Struct(..) => true,
                ItemKind::Union(..) => true,
                ItemKind::Enum(..) => true,
                ItemKind::TyAlias(..) => true,
                _ => false,
            };

            if complete {
                let def_id = cx.node_def_id(i.id);
                name_to_complete.entry(i.ident.name).or_insert_with(Vec::new).push(def_id);
            }
        });

        // (2) Find incomplete type definitions (extern types), and index them by name.
        visit_nodes(krate, |i: &ForeignItem| {
            let incomplete = match i.kind {
                ForeignItemKind::TyAlias(_) => true,
                _ => false,
            };

            if incomplete {
                let def_id = cx.node_def_id(i.id);
                incomplete_to_name.insert(def_id, i.ident.name);
            }
        });

        // (3) Replace references to incomplete types with references to same-named complete types.
        fold_resolved_paths(krate, cx, |qself, path, def| {
            if let Some(&name) = def[0].opt_def_id().as_ref().and_then(|x| incomplete_to_name.get(x)) {
                if let Some(complete_def_ids) = name_to_complete.get(&name) {
                    // Arbitrarily choose the first complete definition, if there's more than one.
                    // A separate transform will canonicalize references to complete types.
                    return (None, cx.def_path(complete_def_ids[0]));
                }
            }
            (qself, path)
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `canonicalize_structs` Command
/// 
/// Usage: `canonicalize_structs`
/// 
/// Marks: `target`
/// 
/// For each type definition marked `target`, delete all other type definitions
/// with the same name, and replace their uses with uses of the `target` type.
/// 
/// This only works when all the identically-named types have the same definition, 
/// such as when all are generated from `#include`s of the same C header.
/// 
/// Example:
/// 
/// ```ignore
///     mod a {
///         pub struct Foo { ... }  // Foo: target
///     }
/// 
///     mod b {
///         struct Foo { ... }  // same as ::a::Foo
/// 
///         unsafe fn use_foo(x: &Foo) { ... }
///     }
/// ```
/// 
/// After running `canonicalize_structs`:
/// 
/// ```ignore
///     mod a {
///         pub struct Foo { ... }
///     }
/// 
///     mod b {
///         // 1. `struct Foo` has been deleted
///         // 2. `use_foo` now references `::a::Foo` directly
///         unsafe fn use_foo(x: &::a::Foo) { ... }
///     }
/// ```
/// 
/// Note that this transform does not check or adjust item visibility.  If the
/// `target` type is not visible throughout the crate, this may introduce compile
/// errors.
pub struct CanonicalizeStructs;

impl Transform for CanonicalizeStructs {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // (1) Find all marked structs.
        let mut canon_ids: HashMap<Symbol, DefId>  = HashMap::new();

        visit_nodes(krate, |i: &Item| {
            if st.marked(i.id, "target") {
                canon_ids.insert(i.ident.name, cx.node_def_id(i.id));
            }
        });

        // (2) Remove all duplicate structs.

        // Map removed struct IDs to their replacements.
        let mut removed_id_map = HashMap::new();

        FlatMapNodes::visit(krate, |i: P<Item>| {
            let should_remove = match i.kind {
                ItemKind::Struct(..) => {
                    if let Some(&canon_def_id) = canon_ids.get(&i.ident.name) {
                        let def_id = cx.node_def_id(i.id);
                        if def_id != canon_def_id {
                            removed_id_map.insert(cx.node_def_id(i.id), canon_def_id);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                },
                _ => false,
            };

            if should_remove {
                smallvec![]
            } else {
                smallvec![i]
            }
        });

        // (3) Remove impls for removed structs.

        FlatMapNodes::visit(krate, |i: P<Item>| {
            let should_remove = match i.kind {
                ItemKind::Impl(box Impl { ref self_ty, .. }) => {
                    if let Some(ty_def_id) = cx.try_resolve_ty(self_ty) {
                        removed_id_map.contains_key(&ty_def_id)
                    } else {
                        false
                    }
                }
                _ => false,
            };

            if should_remove {
                smallvec![]
            } else {
                smallvec![i]
            }
        });

        // (4) Rewrite references to removed structs.

        fold_resolved_paths(krate, cx, |qself, path, def| {
            if let Some(&canon_def_id) = def[0].opt_def_id().as_ref()
                .and_then(|x| removed_id_map.get(&x)) {
                (None, cx.def_path(canon_def_id))
            } else {
                (qself, path)
            }
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("link_funcs", |_args| mk(LinkFuncs));
    reg.register("link_incomplete_types", |_args| mk(LinkIncompleteTypes));
    reg.register("canonicalize_structs", |_args| mk(CanonicalizeStructs));
}
