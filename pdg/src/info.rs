use crate::graph::{Graph, NodeId, NodeKind};
use crate::Graphs;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display, Formatter};

/// Information generated from the PDG proper that is queried by static analysis.
///
/// Includes information about what kinds of [`Node`]s the [`Node`] flows to,
/// and eventually will also include its ability to be used as a `&mut`.
///
/// [`Node`]: crate::graph::Node
#[derive(Hash, Clone, PartialEq, Debug)]
pub struct NodeInfo {
    flows_to: FlowInfo,
}

/// Contains information about what kinds of [`Node`]s a [`Node`] flows to.
/// Load and store kinds contain both Load/Store-Value and Load/Store-Addr.
/// A node A is said to flow into B if it is the transitive 'source' of B.
///
/// [`Node`]: crate::graph::Node
#[derive(Debug, Hash, Clone, Copy, PartialEq, Default)]
pub struct FlowInfo {
    load: Option<NodeId>,
    store: Option<NodeId>,
    pos_offset: Option<NodeId>,
    neg_offset: Option<NodeId>,
}

impl FlowInfo {
    //initializing flow information based on a node's kind
    fn new(n_id: NodeId, k: &NodeKind) -> FlowInfo {
        FlowInfo {
            load: matches!(*k, NodeKind::LoadAddr | NodeKind::LoadValue).then(|| n_id),
            store: matches!(*k, NodeKind::StoreAddr | NodeKind::StoreValue).then(|| n_id),
            pos_offset: matches!(*k, NodeKind::Offset(x) if x > 0).then(|| n_id),
            neg_offset: matches!(*k, NodeKind::Offset(x) if x < 0).then(|| n_id),
        }
    }
}

impl Display for NodeInfo {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "")
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
        });
    }
}

/// Initialize [`Node::info`] for each [`Node`].
///
/// This includes all of the information answering questions of the form "is there a [`Node`] that this is an ancestor of with trait X".
///
/// [`Node`]: crate::graph::Node
/// [`Node::info`]: crate::graph::Node::info
pub fn add_info(pdg: &mut Graphs) {
    for mut g in &mut pdg.graphs {
        set_flow_info(&mut g);
    }
}
