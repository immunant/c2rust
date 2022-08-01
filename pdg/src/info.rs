use std::collections::HashMap;
use crate::graph::{Graphs,GraphId,Graph,NodeId,Node,NodeKind};
use rustc_index::vec::IndexVec;
use std::collections::HashSet;
use std::fmt::{self,Debug, Formatter, Display};

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct NodeInfo {
    flows_to_mutation : Option<NodeId>,
    flows_to_load: Option<NodeId>,
    flows_to_pos_offset: Option<NodeId>,
    flows_to_neg_offset: Option<NodeId>
}

impl Display for NodeInfo {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let fm = match self.flows_to_mutation {
            None => "no mut".to_string(),
            Some(x) => format!("mut {}",x) };
        let fl = match self.flows_to_load {
            None => "no load".to_string(),
            Some(x) => format!("load {}", x)};
        let fpo = match self.flows_to_pos_offset {
            None => "no flow to pos_offset".to_string(),
            Some(_) => "does flow to pos_offset".to_string()};
        let fno = match self.flows_to_neg_offset {
            None => "no flow to neg_offset".to_string(),
            Some(_) => "does flow to neg_offset".to_string()};
        write!(f,"{fm},{fl},{fpo},{fno}")
    }
}

pub fn node_does_mutation (n: &Node) -> bool {
    match n.kind {
        NodeKind::StoreAddr | NodeKind::StoreValue => true,
        _ => false
    }
}

pub fn node_does_load (n: &Node) -> bool {
    match n.kind {
        NodeKind::LoadAddr | NodeKind::LoadValue => true,
        _ => false
    }
}

pub fn node_does_pos_offset (n: &Node) -> bool {
    match n.kind {
        NodeKind::Offset(x) => x>0,
        _ => false
    }
}

pub fn node_does_neg_offset (n: &Node) -> bool {
    match n.kind {
        NodeKind::Offset(x) => x<0,
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
                    if let Some(srcidx) = possible_child_node.source {
                        if seen.contains(&srcidx){
                            to_view.push(possible_child_id);
                        }
                    }
                }
            }
        }
    };
    return None
}

pub fn augment_with_info (pdg: &mut Graphs) {
    let gs : &mut IndexVec<GraphId,Graph> = &mut pdg.graphs;
    for g in gs.iter_mut(){
        let mut idx_flow_to_mut = HashMap::new();
        let mut idx_flow_to_use = HashMap::new();
        let mut idx_flow_to_pos_offset = HashMap::new();
        let mut idx_flow_to_neg_offset = HashMap::new();
        for (idx,_) in g.nodes.iter_enumerated() {
            if let Some(descmutidx) = check_flows_to_node_kind(&g, &idx, node_does_mutation){
                idx_flow_to_mut.insert(idx,descmutidx);
            }
            if let Some(descuseidx) = check_flows_to_node_kind(&g, &idx, node_does_load){
                idx_flow_to_use.insert(idx,descuseidx);
            }
            if let Some(descposoidx) = check_flows_to_node_kind(&g, &idx, node_does_pos_offset){
                idx_flow_to_pos_offset.insert(idx,descposoidx);
            }
            if let Some(descnegoidx) = check_flows_to_node_kind(&g, &idx, node_does_neg_offset){
                idx_flow_to_neg_offset.insert(idx,descnegoidx);
            }
        }
        for (idx,node) in g.nodes.iter_enumerated_mut(){
            node.node_info = Some(NodeInfo {
                flows_to_mutation: idx_flow_to_mut.remove(&idx),
                flows_to_load: idx_flow_to_use.remove(&idx),
                flows_to_pos_offset: idx_flow_to_pos_offset.remove(&idx),
                flows_to_neg_offset: idx_flow_to_pos_offset.remove(&idx)
            })
        }
    }
}
