use crate::graph::{Graph, GraphId, Graphs, Node, NodeId, NodeKind};
use rustc_index::vec::IndexVec;
use rustc_middle::mir::Field;
use std::collections::HashMap;
use std::collections::HashSet;
use itertools::Itertools;
use std::fmt::{self, Debug, Display, Formatter};

/// The information checked in this struct is whether nodes flow to loads, stores, and offsets (pos
/// and neg), as well as whether they are unique.
/// Uniqueness of node X is determined based on whether there is a node Y which is X's ancestor
/// through copies, fields, and offsets, also is a node Z's ancestor through copies, fields, offsets,
/// where the fields are the same between X and Z, and where Z is chronologically between X and X's last descendent.
/// If such a node exists, X is not unique.
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct NodeInfo {
    flows_to_mutation: Option<NodeId>,
    flows_to_load: Option<NodeId>,
    flows_to_pos_offset: Option<NodeId>,
    flows_to_neg_offset: Option<NodeId>,
    non_unique: Option<NodeId>,
}

impl Display for NodeInfo {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = [
            ("mut", self.flows_to_mutation),
            ("load", self.flows_to_load),
            ("+offset", self.flows_to_neg_offset),
            ("-offset", self.flows_to_neg_offset),
            ("non unique by", self.non_unique),
        ].into_iter()
        .filter_map(|(name,node)| Some((name,node?)))
        .format_with(", ", |(name, node), f| f(&format_args!("{name} {node}")));
        write!(f,"{}", s)
    }
}

fn node_does_mutation(n: &Node) -> bool {
    matches!(n.kind, NodeKind::StoreAddr | NodeKind::StoreValue)
}

fn node_does_load(n: &Node) -> bool {
    matches!(n.kind, NodeKind::LoadAddr | NodeKind::LoadValue)
}

fn node_does_pos_offset(n: &Node) -> bool {
    matches!(n.kind, NodeKind::Offset(x) if x > 0)
}

fn node_does_neg_offset(n: &Node) -> bool {
    matches!(n.kind, NodeKind::Offset(x) if x < 0)
}

fn add_children_to_vec(g: &Graph, parents: &HashSet<NodeId>, v: &mut Vec<NodeId>) {
    v.extend(g.nodes
             .iter_enumerated()
             .filter_map(|(id,node)| Some((id,node.source?)))
             .filter(|(_,src_idx)| parents.contains(src_idx))
             .map(|(id,_)| id)
    );
}

fn check_flows_to_node_kind(
    g: &Graph,
    n: &NodeId,
    node_check: fn(&Node) -> bool,
) -> Option<NodeId> {
    let mut seen = HashSet::new();
    let mut to_view = vec![*n];
    while let Some(node_id_to_check) = to_view.pop() {
        if !seen.contains(&node_id_to_check) {
            seen.insert(node_id_to_check);
            let node_to_check: &Node = g.nodes.get(node_id_to_check).unwrap();
            if node_check(node_to_check) {
                return Some(node_id_to_check);
            } else {
                add_children_to_vec(g, &seen, &mut to_view);
            }
        }
    }
    None
}

fn greatest_desc(g: &Graph, n: &NodeId) -> NodeId {
    let mut desc_seen = HashSet::<NodeId>::new();
    let mut to_view = vec![*n];
    let mut greatest_index = n.index();
    let mut greatest_node_idx = *n;
    while let Some(node_id_to_check) = to_view.pop() {
        if !desc_seen.contains(&node_id_to_check) {
            desc_seen.insert(node_id_to_check);
            if node_id_to_check.index() > greatest_index {
                greatest_index = node_id_to_check.index();
                greatest_node_idx = node_id_to_check;
            }
            add_children_to_vec(g, &desc_seen, &mut to_view);
        }
    }
    greatest_node_idx
}

/// Finds the highest-up ancestor of the given node n in g which is reached through Copy, Field, and
/// Offset, and returns its index as well as the Fields through which it is reached, in order
/// (the final element is the Field closest to the returned idx)
fn calc_lineage(g: &Graph, n: &NodeId) -> (NodeId, Vec<Field>) {
    let mut lineage = Vec::new();
    let mut n_idx = *n;
    loop {
        let node = g.nodes.get(n_idx).unwrap();
        let parent = match node.source {
            None => break,
            Some(p) => p,
        };
        n_idx = match node.kind {
            NodeKind::Offset(_) => parent,
            NodeKind::Copy => parent,
            NodeKind::Field(f) => {
                lineage.push(f);
                parent
            }
            _ => break,
        };
    }
    (n_idx, lineage)
}

/// Looks for a node which proves that the given node n is not unique. If any is found, it's
/// immediately returned (no guarantee of which is returned if multiple violate the uniqueness conditions);
/// otherwise None is returned.
pub fn check_whether_rules_obeyed(g: &Graph, n: &NodeId) -> Option<NodeId> {
    let (oldest_ancestor, oldest_lineage) = calc_lineage(g, n);
    let youngest_descendent = greatest_desc(g, n);
    let mut to_view = vec![(oldest_ancestor, oldest_lineage)];
    while let Some((cur_node_id, mut lineage)) = to_view.pop() {
        if cur_node_id == *n {
            continue;
        }
        if let NodeKind::Field(f) = g.nodes.get(cur_node_id).unwrap().kind {
            match lineage.pop() {
                None => continue,
                Some(top_of_vec) => {
                    if top_of_vec != f {
                        continue;
                    }
                }
            }
        }
        if lineage.is_empty()
            && cur_node_id.index() >= n.index()
            && cur_node_id.index() <= youngest_descendent.index()
        {
            return Some(cur_node_id);
        }
        to_view.extend(
            g.nodes
                .iter_enumerated()
                .filter_map(|(id,node)| Some((id,node.source?)))
                .filter(|(_,src_idx)| *src_idx == cur_node_id)
                .map(|(id,_)| (id,lineage.clone()))
        );
    }
    None
}

/// Takes a list of graphs, creates a NodeInfo object for each node in each graph, filling it with
/// the correct flow information.
pub fn augment_with_info(pdg: &mut Graphs) {
    let gs: &mut IndexVec<GraphId, Graph> = &mut pdg.graphs;
    for g in gs.iter_mut() {
        let mut idx_flow_to_mut = HashMap::new();
        let mut idx_flow_to_use = HashMap::new();
        let mut idx_flow_to_pos_offset = HashMap::new();
        let mut idx_flow_to_neg_offset = HashMap::new();
        let mut idx_non_unique = HashMap::new();
        for (idx, _) in g.nodes.iter_enumerated() {
            if let Some(descmutidx) = check_flows_to_node_kind(g, &idx, node_does_mutation) {
                idx_flow_to_mut.insert(idx, descmutidx);
            }
            if let Some(descuseidx) = check_flows_to_node_kind(g, &idx, node_does_load) {
                idx_flow_to_use.insert(idx, descuseidx);
            }
            if let Some(descposoidx) = check_flows_to_node_kind(g, &idx, node_does_pos_offset) {
                idx_flow_to_pos_offset.insert(idx, descposoidx);
            }
            if let Some(descnegoidx) = check_flows_to_node_kind(g, &idx, node_does_neg_offset) {
                idx_flow_to_neg_offset.insert(idx, descnegoidx);
            }
            if let Some(non_unique_idx) = check_whether_rules_obeyed(g, &idx) {
                idx_non_unique.insert(idx, non_unique_idx);
            }
        }
        for (idx, node) in g.nodes.iter_enumerated_mut() {
            node.node_info = Some(NodeInfo {
                flows_to_mutation: idx_flow_to_mut.remove(&idx),
                flows_to_load: idx_flow_to_use.remove(&idx),
                flows_to_pos_offset: idx_flow_to_pos_offset.remove(&idx),
                flows_to_neg_offset: idx_flow_to_pos_offset.remove(&idx),
                non_unique: idx_non_unique.remove(&idx),
            })
        }
    }
}
