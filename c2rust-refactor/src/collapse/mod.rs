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
use rustc_ast::*;
use rustc_span::source_map::Span;
use rustc_span::sym;
use std::collections::HashMap;

mod cfg_attr;
mod deleted;
mod mac_table;
mod macros;
mod node_map;
mod nt_match;

pub use self::cfg_attr::{collect_cfg_attrs, restore_cfg_attrs};
pub use self::deleted::{collect_deleted_nodes, restore_deleted_nodes};
pub use self::mac_table::{collect_macro_invocations, MacInfo, MacTable};
pub use self::macros::collapse_macros;
pub use self::node_map::match_nonterminal_ids;

use crate::command::CommandState;
use crate::node_map::NodeMap;
use deleted::DeletedNode;

pub struct CollapseInfo<'ast> {
    mac_table: MacTable<'ast>,
    cfg_attr_info: HashMap<NodeId, Vec<Attribute>>,
    deleted_info: Vec<DeletedNode<'ast>>,
}

impl<'ast> CollapseInfo<'ast> {
    #[cfg_attr(feature = "profile", flame)]
    pub fn collect(
        unexpanded: &'ast Crate,
        expanded: &'ast Crate,
        node_map: &mut NodeMap,
        cs: &CommandState,
    ) -> Self {
        // Collect info + update node_map, then transfer and commit
        let (mac_table, matched_ids) = collect_macro_invocations(unexpanded, expanded);
        node_map.add_edges(&matched_ids);
        node_map.add_edges(&[(CRATE_NODE_ID, CRATE_NODE_ID)]);
        let cfg_attr_info = collect_cfg_attrs(&unexpanded);
        let deleted_info = collect_deleted_nodes(&unexpanded, &node_map, &mac_table);
        match_nonterminal_ids(node_map, &mac_table);

        node_map.transfer_marks(&mut cs.marks_mut());
        let cfg_attr_info = node_map.transfer_map(cfg_attr_info);
        node_map.commit();

        CollapseInfo {
            mac_table,
            cfg_attr_info,
            deleted_info,
        }
    }

    #[cfg_attr(feature = "profile", flame)]
    pub fn collapse(self, node_map: &mut NodeMap, cs: &CommandState) {
        // Collapse macros + update node_map.  The cfg_attr step requires the updated node_map
        // TODO: we should be able to skip some of these steps if `!cmd_state.krate_changed()`
        collapse_injected(&mut cs.krate_mut());
        let matched_ids = collapse_macros(&mut cs.krate_mut(), &self.mac_table);
        node_map.add_edges(&matched_ids);
        node_map.add_edges(&[(CRATE_NODE_ID, CRATE_NODE_ID)]);

        let cfg_attr_info = node_map.transfer_map(self.cfg_attr_info);
        restore_cfg_attrs(&mut cs.krate_mut(), cfg_attr_info);

        restore_deleted_nodes(
            &mut cs.krate_mut(),
            node_map,
            cs.node_id_counter(),
            self.deleted_info,
        );

        node_map.transfer_marks(&mut cs.marks_mut());
        node_map.commit();
    }
}

/// Returns a list of injected crate names, plus a flag indicating whether a prelude import was
/// also injected.
fn injected_items(krate: &Crate) -> (&'static [&'static str], bool) {
    // Mirrors the logic in syntax::std_inject
    if crate::util::contains_name(&krate.attrs, sym::no_core) {
        (&[], false)
    } else if crate::util::contains_name(&krate.attrs, sym::no_std) {
        if crate::util::contains_name(&krate.attrs, sym::compiler_builtins) {
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
    let mut crate_names = crate_names.to_vec();

    krate.items.retain(|i| {
        match i.kind {
            ItemKind::ExternCrate(_) => {
                // Remove the first `extern crate` matching each entry in `crate_names`.
                if let Some(index) = crate_names.iter().position(|s| i.ident.as_str() == *s) {
                    crate_names.remove(index);
                    false
                } else {
                    true
                }
            }
            ItemKind::Use(_) => {
                if expect_prelude && crate::util::contains_name(&i.attrs, sym::prelude_import) {
                    expect_prelude = false;
                    false
                } else {
                    true
                }
            }
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
