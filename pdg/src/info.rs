use crate::Graphs;
use std::fmt::{self, Debug, Display, Formatter};

/// Information generated from the PDG proper that is queried by static analysis.
///
/// Eventually this will include information about what kinds of [`Node`]s the [`Node`] flows to,
/// as well as its ability to be used as a `&mut`.
///
/// [`Node`]: crate::graph::Node
#[derive(Hash, Clone, PartialEq, Debug)]
pub struct NodeInfo {}

impl Display for NodeInfo {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "")
    }
}

pub fn add_info(pdg: &mut Graphs) {
    for g in &mut pdg.graphs {
        for mut node in &mut g.nodes {
            node.info = Some(NodeInfo {});
        }
    }
}
