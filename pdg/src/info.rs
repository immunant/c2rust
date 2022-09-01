use crate::Graphs;
use crate::graph::{Graph,NodeId,Node,NodeKind};
use std::fmt::{self, Debug, Display, Formatter};
use std::collections::HashMap;

/// Information generated from the PDG proper that is queried by static analysis.
///
/// Includes information about what kinds of [`Node`]s the [`Node`] flows to,
/// and eventually will also include its ability to be used as a `&mut`.
///
/// [`Node`]: crate::graph::Node
#[derive(Hash, Clone, PartialEq, Debug)]
pub struct NodeInfo {
    flows_to: Flows,
}

/// Contains information about what kinds of [`Node`]s the [`Node`] flows to.
/// Load and store kinds contain both 
///
/// [`Node`]: crate::graph::Node.
#[derive(Debug, Hash, Clone, Copy,PartialEq,Default)]
pub struct Flows {
    load: Option<NodeId>,
    store: Option<NodeId>,
    pos_offset: Option<NodeId>,
    neg_offset: Option<NodeId>,
}

impl Display for NodeInfo {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "")
    }
}

fn init_flows(n_id: NodeId, n: &Node) -> Flows {
    Flows {
        load: matches!(n.kind, NodeKind::LoadAddr | NodeKind::LoadValue).then(|| n_id),
        store: matches!(n.kind, NodeKind::StoreAddr | NodeKind::StoreValue).then(|| n_id),
        pos_offset: matches!(n.kind, NodeKind::Offset(x) if x > 0).then(|| n_id),
        neg_offset: matches!(n.kind, NodeKind::Offset(x) if x < 0).then(|| n_id),
    }
}


/// Gathers information from a [`Graph`] (assumed to be acyclic and topologically sorted but not
/// necessarily connected) for each [`Node`] in it whether there is a path following 'source' edges
/// from any [`Node`] with a given property to the [`Node`] in question.
///
/// [`Node`]: crate::graph::Node
/// [`Graph`]: graph::graph::Graph
fn create_flow_info(g: &Graph) -> HashMap<NodeId, Flows> {
    let mut f = HashMap::from_iter(
        g.nodes
            .iter_enumerated()
            .map(|(idx, node)| (idx, init_flows(idx, node))),
    );
    for (n_id, node) in g.nodes.iter_enumerated().rev() {
        let cur: Flows = *(f.get(&n_id).unwrap());
        if let Some(p_id) = node.source {
            let parent = f.get_mut(&p_id).unwrap();
            parent.load = parent.load.or(cur.load);
            parent.store = parent.store.or(cur.store);
            parent.pos_offset = parent.pos_offset.or(cur.pos_offset);
            parent.neg_offset = parent.neg_offset.or(cur.neg_offset);
        }
    }
    f
}

pub fn add_info(pdg: &mut Graphs) {
    for g in &mut pdg.graphs {
        let mut flows = create_flow_info(&g);
        for (n_id,mut node) in g.nodes.iter_enumerated_mut() {
            node.info = Some(NodeInfo { flows_to: flows.remove(&n_id).unwrap()});
        }
    }
}
