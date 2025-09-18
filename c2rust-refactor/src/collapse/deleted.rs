use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_ast::visit::{self, Visitor};
use rustc_ast::*;
use smallvec::SmallVec;
/// Special handling for nodes that were deleted by macro expansion.  This is mostly for `#[cfg]`
/// and `#[test]` attrs.  We collect info on deleted nodes right after expansion, then use that
/// info later to re-insert those nodes during macro collapsing.
use std::collections::{HashMap, HashSet};
use std::mem;

use crate::ast_manip::number_nodes::{number_nodes_with, NodeIdCounter};
use crate::ast_manip::{GetNodeId, ListNodeIds, MutVisit, Visit};
use crate::match_or;
use crate::node_map::NodeMap;

use super::mac_table::{AsMacNodeRef, MacNodeRef, MacTable};

/// Record of a node deletion.
///
/// Some fields of `DeletedNode` can store an ID from any crate (unexpanded, expanded, transformed,
/// collapsed), so when passing or returning `DeletedNode`s, it's important to note which of these
/// crates its IDs are relative to.
#[derive(Clone, Debug)]
pub struct DeletedNode<'ast> {
    /// The ID of the containing node.  This gets updated by `transfer`, progressing through
    /// unexpanded, expanded, and collapsed IDs.
    parent: NodeId,
    /// The IDs of all nodes that preceded this one within `parent`.  When re-inserting, we place
    /// the node after the last node in the transformed AST that corresponds to one of the `preds`.
    /// These get updated by `transfer`.
    preds: Vec<NodeId>,
    /// The original, unexpanded AST of the node itself.
    node: MacNodeRef<'ast>,
    /// Origin info for the node and its children, obtained from `NodeMap::save_origin` on its
    /// unexpanded ID.
    saved_origins: Vec<Option<NodeId>>,
}

struct CollectDeletedNodes<'a, 'ast> {
    table: &'a MacTable<'ast>,
    node_map: &'a NodeMap,
    deleted: Vec<DeletedNode<'ast>>,
}

impl<'a, 'ast> CollectDeletedNodes<'a, 'ast> {
    fn handle_seq<T>(&mut self, parent: NodeId, nodes: &'ast [T])
    where
        T: GetNodeId + ListNodeIds + AsMacNodeRef,
    {
        let mut history = Vec::with_capacity(nodes.len());
        for n in nodes {
            let id = n.get_node_id();
            if self.table.empty_invocs.contains_key(&id) {
                let origins = n
                    .list_node_ids()
                    .into_iter()
                    .map(|id| self.node_map.save_origin(id))
                    .collect();
                self.deleted.push(DeletedNode {
                    parent,
                    preds: history.clone(),
                    node: n.as_mac_node_ref(),
                    saved_origins: origins,
                });
            } else {
                history.push(id);
            }
        }
    }

    fn handle_crate(&mut self, x: &'ast Crate) {
        self.handle_seq(CRATE_NODE_ID, &x.items);
    }
}

impl<'a, 'ast> Visitor<'ast> for CollectDeletedNodes<'a, 'ast> {
    fn visit_expr(&mut self, x: &'ast Expr) {
        match &x.kind {
            ExprKind::Array(elements)
            | ExprKind::Call(_, elements)
            | ExprKind::MethodCall(_, elements, _)
            | ExprKind::Tup(elements) => {
                self.handle_seq(x.id, elements);
            }
            _ => {}
        }
        visit::walk_expr(self, x);
    }

    fn visit_item(&mut self, x: &'ast Item) {
        match x.kind {
            ItemKind::Mod(_, ModKind::Loaded(ref m_items, _, _)) => self.handle_seq(x.id, m_items),
            ItemKind::ForeignMod(ref fm) => self.handle_seq(x.id, &fm.items),
            ItemKind::Trait(box Trait { ref items, .. }) => self.handle_seq(x.id, items),
            ItemKind::Impl(box Impl { ref items, .. }) => self.handle_seq(x.id, items),
            _ => {}
        }
        visit::walk_item(self, x);
    }

    fn visit_block(&mut self, block: &'ast Block) {
        self.handle_seq(block.id, &block.stmts);
        visit::walk_block(self, block);
    }

    fn visit_mac_call(&mut self, mac: &'ast MacCall) {
        visit::walk_mac(self, mac)
    }
}

/// Transfer (in the sense of `NodeMap::transfer`) the `parent` and `preds` IDs of each
/// `DeletedNode` in `dns` across the pending edges of `node_map`.
fn transfer_deleted_nodes<'ast>(
    node_map: &NodeMap,
    dns: Vec<DeletedNode<'ast>>,
) -> Vec<DeletedNode<'ast>> {
    let mut result = Vec::with_capacity(dns.len());

    for mut dn in dns {
        dn.preds = dn
            .preds
            .iter()
            .flat_map(|&id| node_map.transfer(id))
            .collect::<Vec<_>>();

        // Emit a copy of `dn` for each transferred parent ID, avoiding unnecessary clones.
        let mut iter = node_map.transfer(dn.parent).peekable();
        let mut opt_dn = Some(dn);
        while let Some(new_parent) = iter.next() {
            let base_dn = if iter.peek().is_none() {
                opt_dn.take().unwrap()
            } else {
                opt_dn.as_ref().unwrap().clone()
            };
            result.push(DeletedNode {
                parent: new_parent,
                ..base_dn
            });
        }
    }

    result
}

/// Collect info on all nodes deleted from (unexpanded) `krate` by macro expansion.
///
/// The resulting `DeletedNode`s use expanded IDs.
pub fn collect_deleted_nodes<'ast>(
    krate: &'ast Crate,
    node_map: &NodeMap,
    table: &MacTable<'ast>,
) -> Vec<DeletedNode<'ast>> {
    let mut v = CollectDeletedNodes {
        table,
        node_map,
        deleted: Vec::new(),
    };
    v.handle_crate(krate);
    krate.visit(&mut v);
    // The original map stores unexpanded IDs, so transfer it to get expanded IDs instead.
    transfer_deleted_nodes(node_map, v.deleted)
}

struct RestoreDeletedNodes<'a, 'ast> {
    node_map: &'a mut NodeMap,
    counter: &'a NodeIdCounter,
    /// Map of deleted nodes inside each parent, keyed on collapsed parent `NodeId`.
    deleted: HashMap<NodeId, Vec<DeletedNode<'ast>>>,
}

impl<'a, 'ast> RestoreDeletedNodes<'a, 'ast> {
    fn restore_seq<T>(&mut self, parent: NodeId, nodes: &mut Vec<T>)
    where
        T: GetNodeId + ListNodeIds + MutVisit + AsMacNodeRef,
    {
        let deleted = match_or!([self.deleted.get(&parent)]
                                Some(x) => x; return);
        // Set of nodes that are currently present in the parent.  We use this to find the last
        // present node in `preds` for each deleted node.
        let present = nodes
            .iter()
            .map(|x| x.get_node_id())
            .collect::<HashSet<_>>();
        // Maps `NodeId`s in `nodes` to lists of `DeletedNode`s to insert afterward.  The entry for
        // `DUMMY_NODE_ID` gives nodes to insert at the start of `nodes`.
        let mut ins_after: HashMap<NodeId, Vec<&DeletedNode>> = HashMap::new();

        // Note we preserve the order of `deleted` here.  In fact, the order of deleted nodes is
        // preserved through all steps, so deleting multiple consecutive nodes and reinserting them
        // won't mess anything up.
        for dn in deleted {
            let last_pred = dn
                .preds
                .iter()
                .cloned()
                .filter(|&id| present.contains(&id))
                .last()
                .unwrap_or(DUMMY_NODE_ID);
            ins_after.entry(last_pred).or_insert_with(Vec::new).push(dn);
        }

        let result = Vec::with_capacity(nodes.len() + deleted.len());
        let old_nodes = mem::replace(nodes, result);
        let counter = self.counter;
        let node_map = &mut *self.node_map;
        let mut push_ins_after = |nodes: &mut Vec<_>, id| {
            for dn in ins_after.remove(&id).into_iter().flat_map(|x| x) {
                let mut n = T::clone_from_mac_node_ref(dn.node);
                number_nodes_with(&mut n, counter);

                for (id, &origin) in n.list_node_ids().into_iter().zip(&dn.saved_origins) {
                    node_map.restore_origin(id, origin);
                    // Also record that the node ID is not changed by the transform/collapse.
                    node_map.add_edge(id, id);
                }

                nodes.push(n);
            }
        };
        push_ins_after(nodes, DUMMY_NODE_ID);
        for n in old_nodes {
            let id = n.get_node_id();
            nodes.push(n);
            push_ins_after(nodes, id);
        }
    }
}

impl<'a, 'ast> MutVisitor for RestoreDeletedNodes<'a, 'ast> {
    fn visit_crate(&mut self, x: &mut Crate) {
        self.restore_seq(CRATE_NODE_ID, &mut x.items);
        mut_visit::noop_visit_crate(x, self)
    }

    fn flat_map_item(&mut self, mut x: P<Item>) -> SmallVec<[P<Item>; 1]> {
        let id = x.id;
        match x.kind {
            ItemKind::Mod(_, ModKind::Loaded(ref mut m_items, _, _)) => {
                self.restore_seq(id, m_items)
            }
            ItemKind::ForeignMod(ref mut fm) => self.restore_seq(id, &mut fm.items),

            // Both of these contain vectors of P<AssocItem>,
            // and MutVisit has no way to distinguish between them.
            // TODO: when we figure out a way to do so, re-enable them.
            //ItemKind::Trait(box Trait { ref mut items, .. }) => self.restore_seq(id, items),
            //ItemKind::Impl(box Impl { ref mut items, .. }) => self.restore_seq(id, items),
            _ => {}
        }
        mut_visit::noop_flat_map_item(x, self)
    }

    fn visit_expr(&mut self, expr: &mut P<Expr>) {
        let id = expr.id;
        match &mut expr.kind {
            ExprKind::Array(elements)
            | ExprKind::Call(_, elements)
            | ExprKind::MethodCall(_, elements, _)
            | ExprKind::Tup(elements) => {
                self.restore_seq(id, elements);
            }
            _ => {}
        }
        mut_visit::noop_visit_expr(expr, self)
    }

    fn visit_block(&mut self, block: &mut P<Block>) {
        self.restore_seq(block.id, &mut block.stmts);
        mut_visit::noop_visit_block(block, self)
    }

    fn visit_mac_call(&mut self, mac: &mut MacCall) {
        mut_visit::noop_visit_mac(mac, self)
    }
}

/// Convert a flat vector of `DeletedNode`s into a map keyed on parent `NodeId`.
///
/// This preserves relative order inside each sub-`Vec`.
fn index_deleted_nodes<'ast>(
    vec: Vec<DeletedNode<'ast>>,
) -> HashMap<NodeId, Vec<DeletedNode<'ast>>> {
    let mut map = HashMap::new();
    for dn in vec {
        map.entry(dn.parent).or_insert_with(Vec::new).push(dn);
    }
    map
}

/// Restore deleted nodes into the collapsed `krate`.
///
/// The `DeletedNode`s in `deleted` should use expanded IDs, as returned from
/// `collect_deleted_nodes`.
pub fn restore_deleted_nodes(
    krate: &mut Crate,
    node_map: &mut NodeMap,
    counter: &NodeIdCounter,
    deleted: Vec<DeletedNode>,
) {
    // Transfer `deleted` to `collapsed` IDs, which is what `krate` is currently using.
    let deleted = transfer_deleted_nodes(node_map, deleted);
    let deleted = index_deleted_nodes(deleted);

    let mut f = RestoreDeletedNodes {
        node_map,
        counter,
        deleted,
    };
    krate.visit(&mut f)
}
