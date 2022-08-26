use crate::Graphs;
use std::fmt::{self, Debug, Display, Formatter};

///Information generated from the PDG proper which is used as data for static analysis.
///Eventually will include information about what kinds of nodes the node flows to, as
///well as its ability to be used as a mutable reference.
#[derive(Hash, Clone, PartialEq, Debug)]
pub struct NodeInfo {}

impl Display for NodeInfo {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "")
    }
}

pub fn add_flags(pdg: &mut Graphs) {
    for g in &mut pdg.graphs {
        for mut node in &mut g.nodes {
            node.info = Some(NodeInfo {});
        }
    }
}
