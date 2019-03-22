//! Macro collapsing - the opposite of macro expansion.
//!
//! There are four ASTs we care about:
//!
//!  1. Unexpanded: the original AST before macro expansion
//!  2. Expanded: the immediate result of macro expansion, with no further edits
//!  3. Transformed: the result of applying an arbitrary AST transformation to `expanded`
//!  4. Collapsed: `transformed`, but with macro-generated code turned back into macro invocations
//!
//! The general idea here is to compute some kind of delta between `unexpanded` and `expanded`,
//! then apply that in reverse to turn `transformed` into `collapsed`, with some adjustments so we
//! don't lose all the changes that were made to turn `expanded` into `transformed`.
//!
//! Though most of the code and comments talk about "macros", we really mean everything that gets
//! processed during macro expansion, which includes regular macros, proc macros (`format!`, etc.),
//! certain attributes (`#[derive]`, `#[cfg]`), and `std`/prelude injection.
use std::collections::HashSet;
use syntax::attr;
use syntax::ast::*;
use syntax::source_map::Span;
use c2rust_ast_builder::IntoSymbol;

mod cfg_attr;
mod deleted;
mod mac_table;
mod macros;
mod node_map;
mod nt_match;


pub use self::cfg_attr::{collect_cfg_attrs, restore_cfg_attrs};
pub use self::deleted::{collect_deleted_nodes, restore_deleted_nodes};
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

/// Reverse the effect of `std`/prelude injection, by deleting the injected items.
pub fn collapse_injected(krate: &mut Crate) {
    let (crate_names, mut expect_prelude) = injected_items(krate);
    let mut crate_names = crate_names.iter().map(|x| x.into_symbol()).collect::<HashSet<_>>();

    krate.module.items.retain(|i| {
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
    });
}

fn root_callsite_span(sp: Span) -> Span {
    let callsite = sp.source_callsite();
    if callsite == sp {
        sp
    } else {
        root_callsite_span(callsite)
    }
}
