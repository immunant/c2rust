use std::collections::HashMap;
use crate::graph::{Graphs,GraphId,Graph,NodeId,Node,NodeKind};
use rustc_index::vec::IndexVec;
use rustc_middle::mir::Field;
use std::collections::HashSet;
use std::fmt::{self,Debug, Formatter, Display};

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct NodeInfo {
    flows_to_mutation : Option<NodeId>,
    flows_to_load: Option<NodeId>,
    flows_to_pos_offset: Option<NodeId>,
    flows_to_neg_offset: Option<NodeId>,
    non_unique: Option<NodeId>,
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
        let funi = match self.non_unique {
            None => "unique".to_string(),
            Some(x) => format!("non unique proven by {}",x)};
        write!(f,"{fm},{fl},{fpo},{fno},{funi}")
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

fn add_children_to_vec(g: &Graph, parents: & HashSet<NodeId>, v: &mut Vec<NodeId>){
    for (possible_child_id,possible_child_node) in g.nodes.iter_enumerated() {
        if let Some(srcidx) = possible_child_node.source {
            if parents.contains(&srcidx) {
                v.push(possible_child_id);
            }
        }
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
                add_children_to_vec(g,&seen,&mut to_view);
            }
        }
    };
    return None
}

fn greatest_desc(g: &Graph, n:&NodeId) -> NodeId {
    let mut desc_seen = HashSet::<NodeId>::new();
    let mut to_view = Vec::new();
    let mut greatest_index = n.index();
    let mut greatest_node_idx : NodeId = n.clone();
    to_view.push(*n);
    while let Some(node_id_to_check) = to_view.pop(){
        if !desc_seen.contains(&node_id_to_check){
            desc_seen.insert(node_id_to_check);
            if node_id_to_check.index() > greatest_index {
                greatest_index = node_id_to_check.index();
                greatest_node_idx = node_id_to_check;
            }
            add_children_to_vec(g,&desc_seen,&mut to_view);
        }
    }
    return greatest_node_idx;
}

fn calc_lineage(g: &Graph, n:&NodeId) -> (NodeId,Vec<Field>) {
    let mut lineage = Vec::new();
    let mut n_idx = n.clone();
    loop {
        let node = g.nodes.get(n_idx).unwrap();
        let parent = match node.source {
            None => break,
            Some(p) => p
        };
        n_idx = match node.kind {
            NodeKind::Offset(_) => parent,
            NodeKind::Copy => parent,
            NodeKind::Field(f) => {lineage.push(f); parent},
            _ => break
        };
    }
    return (n_idx,lineage);
}

pub fn check_whether_rules_obeyed(g: &Graph, n:&NodeId) -> Option<NodeId>{
    //all descendents die before any alias used = 
    //no non-desc alias used after node but before last descendent
    let (oldest_ancestor,oldest_lineage) = calc_lineage(g,n);
    let youngest_descendent = greatest_desc(g,n);
    let mut to_view = Vec::new();
    to_view.push((oldest_ancestor,oldest_lineage));
    while let Some((cur_node_id,mut lineage)) = to_view.pop(){
        if cur_node_id == *n {
            continue;
        }
        if let NodeKind::Field(f) = g.nodes.get(cur_node_id).unwrap().kind {
            match lineage.pop(){
                None => continue,
                Some(top_of_vec) => {
                    if top_of_vec != f {
                        continue;
                    }
                }
            }
        }
        if lineage.is_empty() && cur_node_id.index() >= n.index() && cur_node_id.index() <= youngest_descendent.index() {
            return Some(cur_node_id);
        }
        for (possible_child_id,possible_child_node) in g.nodes.iter_enumerated() {
            if let Some(srcidx) = possible_child_node.source {
                if cur_node_id == srcidx {
                    to_view.push((possible_child_id,lineage.clone()));
                }
            }
        }
    }
    return None
}

pub fn augment_with_info (pdg: &mut Graphs) {
    let gs : &mut IndexVec<GraphId,Graph> = &mut pdg.graphs;
    for g in gs.iter_mut(){
        let mut idx_flow_to_mut = HashMap::new();
        let mut idx_flow_to_use = HashMap::new();
        let mut idx_flow_to_pos_offset = HashMap::new();
        let mut idx_flow_to_neg_offset = HashMap::new();
        let mut idx_non_unique = HashMap::new();
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
            if let Some(non_unique_idx) = check_whether_rules_obeyed(&g, &idx){
                idx_non_unique.insert(idx,non_unique_idx);
            }
        }
        for (idx,node) in g.nodes.iter_enumerated_mut(){
            node.node_info = Some(NodeInfo {
                flows_to_mutation: idx_flow_to_mut.remove(&idx),
                flows_to_load: idx_flow_to_use.remove(&idx),
                flows_to_pos_offset: idx_flow_to_pos_offset.remove(&idx),
                flows_to_neg_offset: idx_flow_to_pos_offset.remove(&idx),
                non_unique: idx_non_unique.remove(&idx)
            })
        }
    }
}
