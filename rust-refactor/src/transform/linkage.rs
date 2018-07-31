use std::collections::HashMap;
use rustc::hir::def_id::DefId;
use syntax::ast::*;
use syntax::attr;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::util::small_vector::SmallVector;

use api::*;
use ast_manip::fn_edit::{visit_fns, FnKind};
use command::{CommandState, Registry};
use driver::{self, Phase};
use transform::Transform;
use util::HirDefExt;


/// Find this pattern:
///
/// ```text
/// mod a {
///     #[no_mangle] fn f() { ... }
/// }
///
/// mod b {
///     extern "C" { fn f(); }
///     fn g() { ... f() ... }
/// }
/// ```
///
/// And remove the indirection, replacing the call `f()` with `::a::f()`.
pub struct LinkFuncs;

impl Transform for LinkFuncs {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Find all `#[no_mangle]` or `#[export_name=...]` functions, and index them by symbol.
        // (2) Find all extern fns, and index them by def_id.
        let mut symbol_to_def = HashMap::new();
        let mut extern_def_to_symbol = HashMap::new();

        visit_fns(&krate, |fl| {
            let def_id = cx.node_def_id(fl.id);
            if fl.kind != FnKind::Foreign {
                if let Some(name) = attr::first_attr_value_str_by_name(&fl.attrs, "export_name") {
                    symbol_to_def.insert(name, def_id);
                } else if attr::contains_name(&fl.attrs, "no_mangle") {
                    symbol_to_def.insert(fl.ident.name, def_id);
                }
            } else {
                extern_def_to_symbol.insert(def_id, fl.ident.name);
            }
        });

        // (3) Adjust references to extern fns to refer to the `#[no_mangle]` definition instead.
        let krate = fold_resolved_paths(krate, cx, |qself, path, def| {
            if let Some(def_id) = def.opt_def_id() {
                if let Some(&symbol) = extern_def_to_symbol.get(&def_id) {
                    if let Some(&real_def_id) = symbol_to_def.get(&symbol) {
                        return (None, cx.def_path(real_def_id));
                    }
                }
            }
            (qself, path)
        });

        // (4) Remove unused externs
        let krate = fold_nodes(krate, |mut fm: ForeignMod| {
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
            fm
        });

        krate
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// Find this pattern:
///
/// ```text
/// mod a {
///     struct Foo { ... }
/// }
///
/// mod b {
///     enum Foo {}     // incomplete type
/// }
/// ```
///
/// And replace all uses of `b::Foo` with `a::Foo`.
pub struct LinkIncompleteTypes;

impl Transform for LinkIncompleteTypes {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Find complete and incomplete type definitions, and index them by name.  The concept
        // of "incomplete types" doesn't exist in Rust, but Corrode translates incomplete C types
        // as zero-variant enums.
        let mut name_to_complete = HashMap::new();
        let mut incomplete_to_name = HashMap::new();

        visit_nodes(&krate, |i: &Item| {
            let complete = match i.node {
                ItemKind::Struct(..) => Some(true),
                ItemKind::Union(..) => Some(true),
                ItemKind::Enum(ref ed, _) => Some(ed.variants.len() > 0),
                _ => None,
            };

            if let Some(complete) = complete {
                let def_id = cx.node_def_id(i.id);
                if complete {
                    name_to_complete.entry(i.ident.name).or_insert_with(Vec::new).push(def_id);
                } else {
                    incomplete_to_name.insert(def_id, i.ident.name);
                }
            }
        });

        // (2) Replace references to incomplete types with references to same-named complete types.
        fold_resolved_paths(krate, cx, |qself, path, def| {
            if let Some(&name) = def.opt_def_id().as_ref().and_then(|x| incomplete_to_name.get(x)) {
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


/// Replace identically-named structs with a canonical (`target`-marked) version.
pub struct CanonicalizeStructs;

impl Transform for CanonicalizeStructs {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Find all marked structs.
        let mut canon_ids: HashMap<Symbol, DefId>  = HashMap::new();

        visit_nodes(&krate, |i: &Item| {
            if st.marked(i.id, "target") {
                canon_ids.insert(i.ident.name, cx.node_def_id(i.id));
            }
        });

        // (2) Remove all duplicate structs.

        // Map removed struct IDs to their replacements.
        let mut removed_id_map = HashMap::new();

        let krate = fold_nodes(krate, |i: P<Item>| {
            let should_remove = match i.node {
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
                SmallVector::new()
            } else {
                SmallVector::one(i)
            }
        });

        // (3) Remove impls for removed structs.

        let krate = fold_nodes(krate, |i: P<Item>| {
            let should_remove = match i.node {
                ItemKind::Impl(_, _, _, _, _, ref ty, _) => {
                    if let Some(ty_def_id) = cx.try_resolve_ty(ty) {
                        removed_id_map.contains_key(&ty_def_id)
                    } else {
                        false
                    }
                }
                _ => false,
            };

            if should_remove {
                SmallVector::new()
            } else {
                SmallVector::one(i)
            }
        });

        // (4) Rewrite references to removed structs.

        let krate = fold_resolved_paths(krate, cx, |qself, path, def| {
            if let Some(&canon_def_id) = def.opt_def_id().as_ref()
                .and_then(|x| removed_id_map.get(&x)) {
                (None, cx.def_path(canon_def_id))
            } else {
                (qself, path)
            }
        });

        krate
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
