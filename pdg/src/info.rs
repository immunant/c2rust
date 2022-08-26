use crate::Graphs;
use std::fmt::{self, Debug, Formatter, Display};

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
