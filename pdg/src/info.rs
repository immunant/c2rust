use crate::Graphs;

#[derive(Hash, Clone, PartialEq, Debug)]
pub struct NodeInfo {}

pub fn add_flags(pdg: &mut Graphs) {
    for g in &mut pdg.graphs {
        for mut node in &mut g.nodes {
            node.flags = Some(NodeInfo {});
        }
    }
}
