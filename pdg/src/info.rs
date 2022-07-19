use std::collections::HashMap;
use crate::graph::{Graphs,GraphId,Graph,NodeId,Node,NodeKind};
use rustc_index::vec::IndexVec;
use std::collections::HashSet;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct NodeInfo {
    flows_to_mutation : Option<NodeId>,
}

pub fn node_does_mutation (n: &Node) -> bool {
    match n.kind {
        NodeKind::StoreAddr | NodeKind::StoreValue => true,
        _ => false
    }
}

pub fn check_flows_to_node_kind (g : &Graph, n:&NodeId, node_check: fn(&Node)->bool)
    -> Option<NodeId> {
    let mut seen = HashSet::new();
    let mut to_view = Vec::new();
    to_view.push(*n);
    while let Some(node_id_to_check) = to_view.pop(){
        if !seen.contains(&node_id_to_check){
            seen.insert(node_id_to_check);
            let node_to_check : &Node = g.nodes.get(node_id_to_check).unwrap();
            if node_check(node_to_check){
                return Some(node_id_to_check);
            }
            else {
                for (possible_child_id,possible_child_node) in g.nodes.iter_enumerated() {
                    if let Some(_) = possible_child_node.source {
                        to_view.push(possible_child_id);
                    }
                }
            }
        }
    };
    return None
}

pub fn check_flows_to_mut(g: &Graph, n:&NodeId) -> Option<NodeId> {
    return check_flows_to_node_kind(g,n,node_does_mutation);
}

pub fn augment_with_info (pdg: &mut Graphs) {
    let gs : &mut IndexVec<GraphId,Graph> = &mut pdg.graphs;
    for g in gs.iter_mut(){
        let mut idx_flow_to_mut = HashMap::new();
        for (idx,_) in g.nodes.iter_enumerated() {
            if let Some(descmutidx) = check_flows_to_mut(&g, &idx){
                idx_flow_to_mut.insert(idx,descmutidx);
            }
        }
        for (idx,node) in g.nodes.iter_enumerated_mut(){
            node.node_info = Some(NodeInfo {
                flows_to_mutation: idx_flow_to_mut.remove(&idx),
            })
        }
    }
}
