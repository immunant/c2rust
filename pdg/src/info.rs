use crate::graph::{Graph, NodeId, NodeKind};
use crate::Graphs;
use rustc_middle::mir::Field;
use std::cmp::max;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug, Display, Formatter};

/// Information generated from the PDG proper that is queried by static analysis.
///
/// Includes information about what kinds of [`Node`]s the [`Node`] flows to,
/// as well as its ability to be used as a `&mut`.
///
/// [`Node`]: crate::graph::Node
#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub struct NodeInfo {
    flows_to: FlowInfo,
    unique: bool,
}

impl Display for NodeInfo {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "")
    }
}

/// Contains information about what kinds of [`Node`]s a [`Node`] flows to.
/// Load and store kinds contain both Load/Store-Value and Load/Store-Addr.
/// A node A is said to flow into B if it is the transitive 'source' of B.
///
/// [`Node`]: crate::graph::Node
#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, Default)]
pub struct FlowInfo {
    load: Option<NodeId>,
    store: Option<NodeId>,
    pos_offset: Option<NodeId>,
    neg_offset: Option<NodeId>,
}

impl FlowInfo {
    ///Initializes a [`FlowInfo`] based on a node's [`NodeKind`]
    fn new(n_id: NodeId, k: &NodeKind) -> FlowInfo {
        FlowInfo {
            load: matches!(*k, NodeKind::LoadAddr | NodeKind::LoadValue).then(|| n_id),
            store: matches!(*k, NodeKind::StoreAddr | NodeKind::StoreValue).then(|| n_id),
            pos_offset: matches!(*k, NodeKind::Offset(x) if x > 0).then(|| n_id),
            neg_offset: matches!(*k, NodeKind::Offset(x) if x < 0).then(|| n_id),
        }
    }
}

/// Gathers information from a [`Graph`] (assumed to be acyclic and topologically sorted but not
/// necessarily connected) for each [`Node`] in it whether there is a path following 'source' edges
/// from any [`Node`] with a given property to the [`Node`] in question.
///
/// [`Node`]: crate::graph::Node
fn set_flow_info(g: &mut Graph) {
    let mut flow_map: HashMap<NodeId, FlowInfo> = HashMap::from_iter(
        g.nodes
            .iter_enumerated()
            .map(|(idx, node)| (idx, FlowInfo::new(idx, &node.kind))),
    );
    for (n_id, mut node) in g.nodes.iter_enumerated_mut().rev() {
        let cur_node_flow_info: FlowInfo = flow_map.remove(&n_id).unwrap();
        if let Some(p_id) = node.source {
            let parent = flow_map.get_mut(&p_id).unwrap();
            parent.load = parent.load.or(cur_node_flow_info.load);
            parent.store = parent.store.or(cur_node_flow_info.store);
            parent.pos_offset = parent.pos_offset.or(cur_node_flow_info.pos_offset);
            parent.neg_offset = parent.neg_offset.or(cur_node_flow_info.neg_offset);
        }
        node.info = Some(NodeInfo {
            flows_to: cur_node_flow_info,
            unique: false,
        });
    }
}

/// Gathers information from a [`Graph`] (assumed to be acyclic and topologically sorted but not
/// necessarily connected) for each [`Node`] in it what its chronologically (judged by [`NodeId`])
/// final descendent is.
///
/// [`Node`]: crate::graph::Node
fn get_last_desc(g: &mut Graph) -> HashMap<NodeId, NodeId> {
    let mut desc_map: HashMap<NodeId, NodeId> =
        HashMap::from_iter(g.nodes.iter_enumerated().map(|(idx, _)| (idx, idx)));
    for node in g.nodes.iter().rev() {
        if let Some(p_id) = node.source {
            let cur_node_last_desc: NodeId = *desc_map.get(&p_id).unwrap();
            let parent_last_desc: NodeId = desc_map.remove(&p_id).unwrap();
            desc_map.insert(p_id, std::cmp::max(cur_node_last_desc, parent_last_desc));
        }
    }
    desc_map
}

/// Finds the inverse of a [`Graph`], each [`Node`] mapping to a list of its children.
///
/// [`Node`]: crate::graph::Node
fn collect_children(g: &Graph) -> HashMap<NodeId, Vec<NodeId>> {
    let mut m = HashMap::new();
    for (par, chi) in g
        .nodes
        .iter_enumerated()
        .filter_map(|(idx, node)| Some((node.source?, idx)))
    {
        m.entry(par).or_insert_with(Vec::new).push(chi)
    }
    for par in g.nodes.indices() {
        m.try_insert(par, Vec::new());
    }
    m
}

///Given a list of nodes of the same parent and information about them,
///determines if any have conflicts with any of the others.
///Children which are not a field cannot be live at the same time as any other child.
///Children which are a field cannot be live at the same time as any other one of the same field.
fn check_children_conflict(
    g: &Graph,
    n_id: &NodeId,
    children: &HashMap<NodeId, Vec<NodeId>>,
    descs: &HashMap<NodeId, NodeId>,
) -> bool {
    let mut max_descs: HashMap<Option<Field>, NodeId> = HashMap::new();
    for id in children.get(n_id).unwrap() {
        let sib_node = g.nodes.get(*id).unwrap();
        let my_last_desc = descs.get(&id).unwrap().clone();
        if matches!(max_descs.get(&None), Some(max_desc) if max_desc > id)
            || matches!(sib_node.kind,NodeKind::Field(f) if matches!(max_descs.get(&Some(f)),Some(max_desc_field) if max_desc_field > id))
        {
            return true;
        }
        let my_entry: Entry<_, _> = if let NodeKind::Field(f) = sib_node.kind {
            max_descs.entry(Some(f))
        } else {
            max_descs.entry(None)
        };
        my_entry
            .and_modify(|past_last_desc| *past_last_desc = max(*past_last_desc, my_last_desc))
            .or_insert(my_last_desc);
    }
    false
}

fn set_uniqueness(g: &mut Graph) {
    let children = collect_children(g);
    let last_descs = get_last_desc(g);
    let mut uniqueness: HashSet<NodeId> = HashSet::new();
    for n_id in g.nodes.indices() {
        if check_children_conflict(g, &n_id, &children, &last_descs) {
            uniqueness.insert(n_id);
        }
    }
    for (n_id, node) in g.nodes.iter_enumerated_mut() {
        node.info.as_mut().unwrap().unique = uniqueness.contains(&n_id);
    }
}

/// Initialize [`Node::info`] for each [`Node`].
///
/// This includes all of the information answering questions of the form "is there a [`Node`] that this is an ancestor of with trait X".
///
/// [`Node`]: crate::graph::Node
/// [`Node::info`]: crate::graph::Node::info
pub fn add_info(pdg: &mut Graphs) {
    for g in &mut pdg.graphs {
        set_flow_info(g);
        set_uniqueness(g);
    }
}
