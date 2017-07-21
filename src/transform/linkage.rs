use std::collections::HashMap;
use rustc::hir::def_id::DefId;
use rustc::ty::TypeVariants;
use syntax::abi::Abi;
use syntax::ast::*;
use syntax::attr;
use syntax::codemap::Spanned;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

use api::*;
use command::{CommandState, Registry};
use driver::{self, Phase};
use fn_edit::{visit_fns, FnKind};
use fold::Fold;
use transform::Transform;


/// Find this pattern:
///
///     mod a {
///         #[no_mangle] fn f() { ... }
///     }
///     
///     mod b {
///         extern "C" { fn f(); }
///         fn g() { ... f() ... }
///     }
///
/// And remove the indirection, replacing the call `f()` with `::a::f()`.
pub struct LinkFuncs;

impl Transform for LinkFuncs {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Find all `#[no_mangle]` or `#[link_name=...]` functions, and index them by symbol.
        // (2) Find all extern fns, and index them by def_id.
        let mut symbol_to_def = HashMap::new();
        let mut extern_def_to_symbol = HashMap::new();

        visit_fns(&krate, |fl| {
            let def_id = cx.node_def_id(fl.id);
            if fl.kind != FnKind::Foreign {
                if let Some(name) = attr::first_attr_value_str_by_name(&fl.attrs, "link_name") {
                    symbol_to_def.insert(name, def_id);
                } else if attr::contains_name(&fl.attrs, "no_mangle") {
                    symbol_to_def.insert(fl.ident.name, def_id);
                }
            } else {
                extern_def_to_symbol.insert(def_id, fl.ident.name);
            }
        });

        // (3) Adjust references to extern fns to refer to the `#[no_mangle]` definition instead.
        let krate = fold_resolved_paths(krate, cx, |qself, path, def_id| {
            if let Some(&symbol) = extern_def_to_symbol.get(&def_id) {
                if let Some(&real_def_id) = symbol_to_def.get(&symbol) {
                    return (None, cx.def_path(real_def_id));
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
///     mod a {
///         struct Foo { ... }
///     }
///     
///     mod b {
///         enum Foo {}     // incomplete type
///     }
///
/// And replace all uses of `b::Foo` with `a::Foo`.
pub struct LinkIncompleteTypes;

impl Transform for LinkIncompleteTypes {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
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
        fold_resolved_paths(krate, cx, |qself, path, def_id| {
            if let Some(&name) = incomplete_to_name.get(&def_id) {
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


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("link_funcs", |_args| mk(LinkFuncs));
    reg.register("link_incomplete_types", |_args| mk(LinkIncompleteTypes));
}
