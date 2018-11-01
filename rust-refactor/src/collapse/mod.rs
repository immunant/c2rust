use std::collections::HashSet;
use syntax::attr;
use syntax::ast::*;
use syntax::source_map::Span;
use rust_ast_builder::IntoSymbol;

mod cfg_attr;
mod mac_table;
mod macros;
mod node_map;
mod nt_match;


pub use self::cfg_attr::{collect_cfg_attrs, restore_cfg_attrs};
pub use self::mac_table::{MacTable, MacInfo, collect_macro_invocations};
pub use self::node_map::match_nonterminal_ids;
pub use self::macros::collapse_macros;

/// Returns a list of injected crate names, plus a flag indicating whether a prelude import was
/// also injected.
fn injected_items(krate: &Crate) -> (&'static [&'static str], bool) {
    // Mirrors the logic in syntax::std_inject
    if attr::contains_name(&krate.attrs, "no_core") {
        (&[], false)
    } else if attr::contains_name(&krate.attrs, "no_std") {
        if attr::contains_name(&krate.attrs, "compiler_builtins") {
            (&["core"], true)
        } else {
            (&["core", "compiler_builtins"], true)
        }
    } else {
        (&["std"], true)
    }
}

pub fn collapse_injected(mut krate: Crate) -> Crate {
    let (crate_names, mut expect_prelude) = injected_items(&krate);
    let mut crate_names = crate_names.iter().map(|x| x.into_symbol()).collect::<HashSet<_>>();

    let new_items = krate.module.items.into_iter().filter(|i| {
        match i.node {
            ItemKind::ExternCrate(_) => {
                // Remove the first `extern crate` matching each entry in `crate_names`.
                if crate_names.remove(&i.ident.name) {
                    false
                } else {
                    true
                }
            },
            ItemKind::Use(_) => {
                if expect_prelude && attr::contains_name(&i.attrs, "prelude_import") {
                    expect_prelude = false;
                    false
                } else {
                    true
                }
            },
            _ => true,
        }
    }).collect();
    krate.module.items = new_items;
    krate
}

fn root_callsite_span(sp: Span) -> Span {
    let callsite = sp.source_callsite();
    if callsite == sp {
        sp
    } else {
        root_callsite_span(callsite)
    }
}
