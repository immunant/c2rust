use crate::graph::{Graph, Node, NodeId, NodeKind};
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
fn get_last_desc(g: &mut Graph) -> HashMap<NodeId, NodeId> {
    let mut desc_map: HashMap<NodeId, NodeId> =
        HashMap::from_iter(g.nodes.iter_enumerated().map(|(idx, _)| (idx, idx)));
    for (n_id, node) in g.nodes.iter_enumerated().rev() {
        if let Some(p_id) = node.source {
            let cur_node_last_desc: NodeId = *desc_map.get(&n_id).unwrap();
            let parent_last_desc: NodeId = desc_map.remove(&p_id).unwrap();
            desc_map.insert(p_id, std::cmp::max(cur_node_last_desc, parent_last_desc));
        }
    }
    desc_map
}

/// Finds the inverse of a [`Graph`], each [`Node`] mapping to a list of its children.
fn collect_children(g: &Graph) -> HashMap<NodeId, Vec<NodeId>> {
    let mut m = HashMap::new();
    for par in g.nodes.indices() {
        let _ = m.try_insert(par, Vec::new());
    }
    for (par, chi) in g
        .nodes
        .iter_enumerated()
        .filter_map(|(idx, node)| Some((node.source?, idx)))
    {
        m.get_mut(&par).unwrap().push(chi);
    }
    m
}

/// Given a list of nodes of the same parent and information about them,
/// determines if any have conflicts with any of the others.
/// Children which are not a field cannot be live at the same time as any other child.
/// Children which are a field cannot be live at the same time as any other one of the same field.
fn check_children_conflict(
    g: &Graph,
    n_id: &NodeId,
    children: &HashMap<NodeId, Vec<NodeId>>,
    descs: &HashMap<NodeId, NodeId>,
) -> bool {
    let mut max_descs: HashMap<Option<Field>, NodeId> = HashMap::new();
    for id in children.get(n_id).unwrap() {
        let sib_node: &Node = g.nodes.get(*id).unwrap();
        let my_last_desc = descs.get(&id).unwrap().clone();
        //if the first below matches, then two siblings, neither a field, conflict
        //if the second matches, then two siblings of the same field conflict
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
    let mut nonuniqueness: HashSet<NodeId> = HashSet::new();
    for (n_id, node) in g.nodes.iter_enumerated() {
        if matches!(node.source, Some(p_id) if nonuniqueness.contains(&p_id))
            || check_children_conflict(g, &n_id, &children, &last_descs)
        {
            nonuniqueness.insert(n_id);
        }
    }
    for (n_id, node) in g.nodes.iter_enumerated_mut() {
        node.info.as_mut().unwrap().unique = !nonuniqueness.contains(&n_id);
    }
}

/// Initialize [`Node::info`] for each [`Node`].
///
/// This includes all of the information answering questions of the form "is there a [`Node`] that
/// this is an ancestor of with trait X", as well as whether the node can be considered unique.
pub fn add_info(pdg: &mut Graphs) {
    for g in &mut pdg.graphs {
        set_flow_info(g);
        set_uniqueness(g);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use c2rust_analysis_rt::mir_loc::Func;
    use rustc_middle::mir::Local;

    fn mk_node(g: &mut Graph, kind: NodeKind, source: Option<NodeId>) -> NodeId {
        g.nodes.push(Node {
            function: Func {
                def_path_hash: (1, 2).into(),
                name: "fake_function".into(),
            },
            block: 0_u32.into(),
            statement_idx: 0,
            dest: None,
            kind,
            source,
            info: None,
            debug_info: "".into(),
        })
    }

    fn mk_addr_of_local(g: &mut Graph, local: impl Into<Local>) -> NodeId {
        mk_node(g, NodeKind::AddrOfLocal(local.into()), None)
    }

    fn mk_copy(g: &mut Graph, source: NodeId) -> NodeId {
        mk_node(g, NodeKind::Copy, Some(source))
    }

    fn mk_store_addr(g: &mut Graph, source: NodeId) -> NodeId {
        mk_node(g, NodeKind::StoreAddr, Some(source))
    }

    fn mk_field(g: &mut Graph, source: NodeId, field: impl Into<Field>) -> NodeId {
        mk_node(g, NodeKind::Field(field.into()), Some(source))
    }

    fn build_pdg(g: Graph) -> Graphs {
        let mut pdg = Graphs::default();
        pdg.graphs.push(g);
        add_info(&mut pdg);
        pdg
    }

    fn info(pdg: &Graphs, id: NodeId) -> &NodeInfo {
        pdg.graphs[0_u32.into()].nodes[id].info.as_ref().unwrap()
    }

    #[test]
    fn unique_interleave() {
        let mut g = Graph::default();
        // let mut a = 0;   // A
        // let b = &mut a;  // B1
        // *b = 0;          // B2
        // let c = &mut a;  // C1
        // *c = 0;          // C2
        // *b = 0;          // B3
        // *c = 0;          // C3
        //
        // A
        // +----.
        // B1   |
        // +-B2 |
        // |    C1
        // |    +-C2
        // B3   |
        //      C3
        let a = mk_addr_of_local(&mut g, 0_u32);
        let b1 = mk_copy(&mut g, a);
        let b2 = mk_store_addr(&mut g, b1);
        let c1 = mk_copy(&mut g, a);
        let c2 = mk_store_addr(&mut g, c1);
        let b3 = mk_store_addr(&mut g, b1);
        let c3 = mk_store_addr(&mut g, c1);

        let pdg = build_pdg(g);
        assert!(!info(&pdg, a).unique);
        assert!(!info(&pdg, b1).unique);
        assert!(!info(&pdg, b2).unique);

        assert!(!info(&pdg, b3).unique);
        assert!(!info(&pdg, c1).unique);
        assert!(!info(&pdg, c2).unique);
        assert!(!info(&pdg, c3).unique);
    }

    #[test]
    fn unique_interleave_onesided() {
        let mut g = Graph::default();
        // let mut a = 0;   // A
        // let b = &mut a;  // B1
        // *b = 0;          // B2
        // let c = &mut a;  // C1
        // *c = 0;          // C2
        // *b = 0;          // B3
        //
        // A
        // +----.
        // B1   |
        // +-B2 |
        // |    C1
        // |    +-C2
        // B3
        let a = mk_addr_of_local(&mut g, 0_u32);
        let b1 = mk_copy(&mut g, a);
        let b2 = mk_store_addr(&mut g, b1);
        let c1 = mk_copy(&mut g, a);
        let c2 = mk_store_addr(&mut g, c1);
        let b3 = mk_store_addr(&mut g, b1);

        let pdg = build_pdg(g);
        assert!(!info(&pdg, a).unique);
        assert!(!info(&pdg, b1).unique);
        assert!(!info(&pdg, b2).unique);
        assert!(!info(&pdg, b3).unique);
        assert!(!info(&pdg, c1).unique);
        assert!(!info(&pdg, c2).unique);
    }

    #[test]
    fn unique_sub_borrow() {
        let mut g = Graph::default();
        // let mut a = 0;   // A
        // let b = &mut a;  // B1
        // *b = 0;          // B2
        // let c = &mut *b; // C1
        // *c = 0;          // C2
        // *b = 0;          // B3
        //
        // A
        // |
        // B1
        // +-B2
        // +----C1
        // |    +-C2
        // B3
        let a = mk_addr_of_local(&mut g, 0_u32);
        let b1 = mk_copy(&mut g, a);
        let b2 = mk_store_addr(&mut g, b1);
        let c1 = mk_copy(&mut g, b1);
        let c2 = mk_store_addr(&mut g, c1);
        let b3 = mk_store_addr(&mut g, b1);

        let pdg = build_pdg(g);
        assert!(info(&pdg, a).unique);
        assert!(info(&pdg, b1).unique);
        assert!(info(&pdg, b2).unique);
        assert!(info(&pdg, b3).unique);
        assert!(info(&pdg, c1).unique);
        assert!(info(&pdg, c2).unique);
    }

    #[test]
    fn unique_sub_borrow_bad() {
        let mut g = Graph::default();
        // let mut a = 0;   // A
        // let b = &mut a;  // B1
        // *b = 0;          // B2
        // let c = &mut *b; // C1
        // *c = 0;          // C2
        // *b = 0;          // B3
        // *c = 0;          // C3
        //
        // A
        // |
        // B1
        // +-B2
        // +----C1
        // |    +-C2
        // B3   |
        //      C3
        let a = mk_addr_of_local(&mut g, 0_u32);
        let b1 = mk_copy(&mut g, a);
        let b2 = mk_store_addr(&mut g, b1);
        let c1 = mk_copy(&mut g, b1);
        let c2 = mk_store_addr(&mut g, c1);
        let b3 = mk_store_addr(&mut g, b1);
        let c3 = mk_store_addr(&mut g, c1);

        let pdg = build_pdg(g);
        assert!(info(&pdg, a).unique);
        assert!(!info(&pdg, b1).unique);
        assert!(!info(&pdg, b2).unique);
        assert!(!info(&pdg, b3).unique);
        assert!(!info(&pdg, c1).unique);
        assert!(!info(&pdg, c2).unique);
        assert!(!info(&pdg, c3).unique);
    }

    #[test]
    fn okay_use_different_fields() {
        let mut g = Graph::default();
        // let mut a = Point {x: 0, y:0}; // A
        // let b = &mut a.x;              // B1
        // let c = &mut a.y;              // C1
        // *b = 1;                        // B2
        // *c = 2;                        // C2
        //
        // A
        // |-------
        // |x     |
        // B1     |y
        // |      C1
        // B2     |
        //        C2
        let a = mk_addr_of_local(&mut g, 0_u32);
        let b11 = mk_field(&mut g, a, 0_u32);
        let b1 = mk_copy(&mut g, b11);
        let c11 = mk_field(&mut g, a, 1_u32);
        let c1 = mk_copy(&mut g, c11);
        let b2 = mk_store_addr(&mut g, b1);
        let c2 = mk_store_addr(&mut g, c1);

        let pdg = build_pdg(g);
        assert!(info(&pdg, a).unique);
        assert!(info(&pdg, b1).unique);
        assert!(info(&pdg, b2).unique);
        assert!(info(&pdg, c1).unique);
        assert!(info(&pdg, c2).unique);
    }

    #[test]
    fn same_fields_cousins() {
        let mut g = Graph::default();
        // let mut a = Point {x: 0, y:0}; // A
        // let j = &mut a;                // J
        // let b = &mut j.x;              // B1
        // let c = &mut j.x;              // C1
        // *b = 1;                        // B2
        // *c = 2;                        // C2
        //
        // A
        // |-----------
        // J          |
        // |-------   |
        // |x     |   |
        // B1     |x  |
        // |      C1  |y
        // |      |   |
        // B2     |   |
        //        C2  |
        //            D1
        let a = mk_addr_of_local(&mut g, 0_u32);
        let j = mk_copy(&mut g, a);
        let b11 = mk_field(&mut g, j, 0_u32);
        let b1 = mk_copy(&mut g, b11);
        let c11 = mk_field(&mut g, j, 0_u32);
        let c1 = mk_copy(&mut g, c11);
        let b2 = mk_store_addr(&mut g, b1);
        let c2 = mk_store_addr(&mut g, c1);
        let d1 = mk_field(&mut g, a, 1_u32);

        let pdg = build_pdg(g);
        assert!(info(&pdg, a).unique);
        assert!(!info(&pdg, j).unique);
        assert!(!info(&pdg, b1).unique);
        assert!(!info(&pdg, b2).unique);
        assert!(!info(&pdg, c1).unique);
        assert!(!info(&pdg, c2).unique);
        assert!(info(&pdg, d1).unique);
    }

    #[test]
    fn field_vs_raw() {
        let mut g = Graph::default();
        // let mut a = Point {x: 0, y:0}; // A
        // let b = &mut a;                // B1
        // let c = &mut a.y;              // C1
        // *b = 1;                        // B2
        // *c = 2;                        // C2
        //
        // A
        // |-------
        // |      |
        // B1     |y
        // |      C1
        // |      C2
        // B2
        let a = mk_addr_of_local(&mut g, 0_u32);
        let b1 = mk_copy(&mut g, a);
        let c11 = mk_field(&mut g, a, 1_u32);
        let c1 = mk_copy(&mut g, c11);
        let c2 = mk_store_addr(&mut g, c1);
        let b2 = mk_store_addr(&mut g, b1);

        let pdg = build_pdg(g);
        assert!(!info(&pdg, a).unique);
        assert!(!info(&pdg, b1).unique);
        assert!(!info(&pdg, b2).unique);
        assert!(!info(&pdg, c1).unique);
        assert!(!info(&pdg, c2).unique);
    }

    #[test]
    fn fields_different_levels() {
        let mut g = Graph::default();
        // let mut a = Point {x: 0, y:0}; // A
        // let b = &mut a;                // B1
        // let c = &mut b.y;              // C1
        // let bb = &mut b.y;             // B2
        // *b = 1;                        // B3
        // *c = 2;                        // C
        //
        // A
        // |-------
        // |      |
        // B1     |y
        // |      C1
        // |y
        // B2

        let a = mk_addr_of_local(&mut g, 0_u32);
        let b1 = mk_copy(&mut g, a);
        let c1 = mk_field(&mut g, a, 1_u32);
        let b2 = mk_field(&mut g, b1, 1_u32);
        let c2 = mk_store_addr(&mut g, c1);

        let pdg = build_pdg(g);
        assert!(!info(&pdg, a).unique);
        assert!(!info(&pdg, b1).unique);
        assert!(!info(&pdg, b2).unique);
        assert!(!info(&pdg, c1).unique);
        assert!(!info(&pdg, c2).unique);
        assert!(!info(&pdg, b2).unique);
    }

    #[test]
    fn lots_of_siblings() {
        let mut g = Graph::default();
        // let mut a = ColorPoint {x: 0, y: 0, z: Color{ r: 100, g: 100, b: 100}};   //A1
        // let b = &mut a.x;                                                         //B1
        // let c = &mut a.y;                                                         //C1
        // a.z.r = 200;                                                              //A2(X1-3)
        // *b = 4;                                                                   //B2
        // *c = 2;                                                                   //C2
        // let d = &mut a;                                                           //D1
        // *d = ColorPoint {x: 0, y: 0, z: Color {r: 20, g:200, b: 20}};             //D2
        // let e = &mut a.z;                                                         //E
        // let f = &mut e.g;                                                         //F1
        // let g = &mut e.g;                                                         //G
        // *f = 3;                                                                   //F2
        // a.z.r = 100;                                                              //A3(X4-6)

        let (x, y, z) = (0_u32, 1_u32, 2_u32);
        let (red, green, _blue) = (0_u32, 1_u32, 2_u32);

        let a = mk_addr_of_local(&mut g, 0_u32);
        let b1 = mk_field(&mut g, a, x);
        let c1 = mk_field(&mut g, a, y);
        let x1 = mk_field(&mut g, a, z);
        let x2 = mk_field(&mut g, x1, red);
        let x3 = mk_store_addr(&mut g, x2);
        let b2 = mk_store_addr(&mut g, b1);
        let c2 = mk_store_addr(&mut g, c1);
        let d1 = mk_copy(&mut g, a);
        let d2 = mk_store_addr(&mut g, d1);
        let e = mk_field(&mut g, a, z);
        let f1 = mk_field(&mut g, e, green);
        let gg = mk_field(&mut g, e, green);
        let f2 = mk_store_addr(&mut g, f1);
        let x4 = mk_field(&mut g, a, z);
        let x5 = mk_field(&mut g, x4, green);
        let x6 = mk_store_addr(&mut g, x5);

        let pdg = build_pdg(g);

        assert!(info(&pdg, a).unique);
        assert!(info(&pdg, b1).unique);
        assert!(info(&pdg, c1).unique);
        assert!(info(&pdg, x1).unique);
        assert!(info(&pdg, x2).unique);
        assert!(info(&pdg, x3).unique);
        assert!(info(&pdg, b2).unique);
        assert!(info(&pdg, c2).unique);
        assert!(info(&pdg, d1).unique);
        assert!(info(&pdg, d2).unique);
        assert!(!info(&pdg, e).unique);
        assert!(!info(&pdg, f1).unique);
        assert!(!info(&pdg, gg).unique);
        assert!(!info(&pdg, f2).unique);
        assert!(info(&pdg, x4).unique);
        assert!(info(&pdg, x5).unique);
        assert!(info(&pdg, x6).unique);
    }
}
