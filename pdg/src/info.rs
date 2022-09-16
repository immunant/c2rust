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
#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub struct NodeInfo {
    flows_to: FlowInfo,

    /// Whether the [`Node`] can be used as a `&mut`.
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
    /// Initializes a [`FlowInfo`] based on a [`Node`]'s [`NodeKind`]
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

/// Given a list of [`Node`]s of the same parent and information about them,
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
        let my_last_desc = *descs.get(id).unwrap();
        // if the first below matches, then two siblings, neither a field, conflict
        // if the second matches, then two siblings of the same field conflict
        if matches!(max_descs.get(&None), Some(max_desc) if max_desc > id)
            || matches!(sib_node.kind, NodeKind::Field(f) if matches!(max_descs.get(&Some(f)), Some(max_desc_field) if max_desc_field > id))
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
        // If a node is not unique, none of its descendents can be unique.
        // If any of a node's children conflict with each other, it is not unique.
        // Because we traverse the graph visiting all parents before their children,
        // just checking the immediate parent's uniqueness status is sufficient to guarantee the
        // first condition.
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

    #[derive(Debug)]
    struct CheckedNode {
        id: NodeId,
        unique: bool,
        stmt: &'static str,
    }

    #[derive(Debug, Default)]
    struct CheckedGraph {
        graph: Graph,
        checked_nodes: Vec<CheckedNode>,
    }

    impl CheckedGraph {
        fn mk_node(
            &mut self,
            stmt: &'static str,
            unique: bool,
            kind: NodeKind,
            source: Option<NodeId>,
        ) -> NodeId {
            let id = self.graph.nodes.push(Node {
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
                debug_info: stmt.into(),
            });
            let checked_node = CheckedNode { id, unique, stmt };
            self.checked_nodes.push(checked_node);
            id
        }

        fn mk_addr_of_local(
            &mut self,
            smt: &'static str,
            unique: bool,
            local: impl Into<Local>,
        ) -> NodeId {
            self.mk_node(smt, unique, NodeKind::AddrOfLocal(local.into()), None)
        }

        fn mk_copy(&mut self, smt: &'static str, unique: bool, source: NodeId) -> NodeId {
            self.mk_node(smt, unique, NodeKind::Copy, Some(source))
        }

        fn mk_store_addr(&mut self, smt: &'static str, unique: bool, source: NodeId) -> NodeId {
            self.mk_node(smt, unique, NodeKind::StoreAddr, Some(source))
        }

        fn mk_field(
            &mut self,
            stmt: &'static str,
            unique: bool,
            source: NodeId,
            field: impl Into<Field>,
        ) -> NodeId {
            self.mk_node(stmt, unique, NodeKind::Field(field.into()), Some(source))
        }

        fn mk_offset(
            &mut self,
            stmt: &'static str,
            unique: bool,
            source: NodeId,
            i: isize,
        ) -> NodeId {
            self.mk_node(stmt, unique, NodeKind::Offset(i), Some(source))
        }

        fn check_unique(self) {
            let (pdg, gid) = {
                let mut pdg = Graphs::default();
                let gid = pdg.graphs.push(self.graph);
                add_info(&mut pdg);
                (pdg, gid)
            };
            for CheckedNode { id, unique, stmt } in self.checked_nodes.into_iter().rev() {
                let info = pdg.graphs[gid].nodes[id].info.as_ref().unwrap();
                let non = if unique { "" } else { "non-" };
                assert_eq!(
                    info.unique, unique,
                    "expected {id} (`{stmt};`) to be {non}unique in {pdg}"
                );
            }
        }
    }

    /// ```rust
    /// let mut a = 0;
    /// let b = &mut a;
    /// *b = 0;
    /// let c = &mut a;
    /// *c = 0;
    /// *b = 0;
    /// *c = 0;
    /// ```
    ///
    /// ```text
    /// A
    /// +----.
    /// B1   |
    /// +-B2 |
    /// |    C1
    /// |    +-C2
    /// B3   |
    ///      C3
    /// ```
    #[test]
    fn unique_interleave() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = 0", false, 0_u32);
        let b1 = g.mk_copy("let b = &mut a", false, a);
        g.mk_store_addr("*b = 0", false, b1);
        let c1 = g.mk_copy("let c = &mut a", false, a);
        g.mk_store_addr("*c = 0", false, c1);
        g.mk_store_addr("*b = 0", false, b1);
        g.mk_store_addr("*c = 0", false, c1);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = 0;   // A
    /// let b = &mut a;  // B1
    /// *b = 0;          // B2
    /// let c = &mut a;  // C1
    /// *c = 0;          // C2
    /// *b = 0;          // B3
    /// ```
    ///
    /// ```text
    /// A
    /// +----.
    /// B1   |
    /// +-B2 |
    /// |    C1
    /// |    +-C2
    /// B3
    /// ```
    #[test]
    fn unique_interleave_onesided() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = 0", false, 0_u32);
        let b1 = g.mk_copy("let b = &mut a", false, a);
        g.mk_store_addr("*b = 0", false, b1);
        let c1 = g.mk_copy("let c = &mut a", false, a);
        g.mk_store_addr("*c = 0", false, c1);
        g.mk_store_addr("*b = 0", false, b1);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = 0;
    /// let b = &mut a;
    /// *b = 0;
    /// let c = &mut *b;
    /// *c = 0;
    /// *b = 0;
    /// ```
    ///
    /// ```text
    /// A
    /// |
    /// B1
    /// +-B2
    /// +----C1
    /// |    +-C2
    /// B3
    /// ```
    #[test]
    fn unique_sub_borrow() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = 0", true, 0_u32);
        let b1 = g.mk_copy("let b = &mut a", true, a);
        g.mk_store_addr("*b = 0", true, b1);
        let c1 = g.mk_copy("let c = &mut *b", true, b1);
        g.mk_store_addr("*c = 0", true, c1);
        g.mk_store_addr("*c = 0", true, b1);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = 0;
    /// let b = &mut a;
    /// *b = 0;
    /// let c = &mut *b;
    /// *c = 0;
    /// *b = 0;
    /// *c = 0;
    /// ```
    ///
    /// ```text
    /// A
    /// |
    /// B1
    /// +-B2
    /// +----C1
    /// |    +-C2
    /// B3   |
    ///      C3
    /// ```
    #[test]
    fn unique_sub_borrow_bad() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = 0", true, 0_u32);
        let b1 = g.mk_copy("let b = &mut a", false, a);
        g.mk_store_addr("*b = 0", false, b1);
        let c1 = g.mk_copy("let c = &mut *b", false, b1);
        g.mk_store_addr("*c = 0", false, c1);
        g.mk_store_addr("*b = 0", false, b1);
        g.mk_store_addr("*c = 0", false, c1);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = Point { x: 0, y: 0 };
    /// let b = &mut a.x;
    /// let c = &mut a.y;
    /// *b = 1;
    /// *c = 2;
    /// ```
    ///
    /// ```text
    /// A
    /// |-------
    /// |x     |
    /// B1     |y
    /// |      C1
    /// B2     |
    ///        C2
    /// ```
    #[test]
    fn okay_use_different_fields() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = Point { x: 0, y: 0 }", true, 0_u32);
        let b11 = g.mk_field("let b = &mut a.x", true, a, 0_u32);
        let b1 = g.mk_copy("let b = &mut a.x", true, b11);
        let c11 = g.mk_field("let c = &mut a.y", true, a, 1_u32);
        let c1 = g.mk_copy("let c = &mut a.y", true, c11);
        g.mk_store_addr("*b = 1", true, b1);
        g.mk_store_addr("*c = 2", true, c1);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = Point { x: 0, y: 0 };
    /// let j = &mut a;
    /// let b = &mut j.x;
    /// let c = &mut j.x;
    /// *b = 1;
    /// *c = 2;
    /// *(a.y) = 3;
    /// ```
    ///
    /// ```text
    /// A
    /// |-----------
    /// J          |
    /// |-------   |
    /// |x     |   |
    /// B1     |x  |
    /// |      C1  |y
    /// |      |   |
    /// B2     |   |
    ///        C2  |
    ///            D1
    /// ```
    #[test]
    fn same_fields_cousins() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = Point { x: 0, y: 0 }", true, 0_u32);
        let j = g.mk_copy("let j = &mut a", false, a);
        let b11 = g.mk_field("let b = &mut j.x", false, j, 0_u32);
        let b1 = g.mk_copy("let b = &mut j.x", false, b11);
        let c11 = g.mk_field("let c = &mut j.x", false, j, 0_u32);
        let c1 = g.mk_copy("let c = &mut j.x", false, c11);
        g.mk_store_addr("*b = 1", false, b1);
        g.mk_store_addr("*c = 2", false, c1);
        let d1 = g.mk_field("*(a.y) = 3", true, a, 1_u32);
        g.mk_store_addr("*(a.y) = 3", true, d1);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = Point { x: 0, y: 0 };
    /// let b = &mut a;
    /// let c = &mut a.y;
    /// *c = 2;
    /// *b = 1;
    /// ```
    ///
    /// ```text
    /// A
    /// |-------
    /// |      |
    /// B1     |y
    /// |      C1
    /// |      C2
    /// B2
    /// ```
    #[test]
    fn field_vs_raw() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = Point { x: 0, y: 0 }", false, 0_u32);
        let b1 = g.mk_copy("let b = &mut a", false, a);
        let c11 = g.mk_field("let c = &mut a.y", false, a, 1_u32);
        let c1 = g.mk_copy("let c = &mut a.y", false, c11);
        g.mk_store_addr("*c = 2", false, c1);
        g.mk_store_addr("*b = 1", false, b1);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = Point { x: 0, y: 0 };
    /// let b = &mut a;
    /// let c = &mut b.y;
    /// let bb = &mut b.y;
    /// *c = 2;
    /// *bb = 1;
    /// ```
    ///
    /// ```text
    /// A
    /// |-------
    /// |      |
    /// B1     |y
    /// |      C1
    /// |y
    /// B2
    /// ```
    #[test]
    fn fields_different_levels() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = Point { x: 0, y: 0 }", false, 0_u32);
        let b1 = g.mk_copy("let b = &mut a", false, a);
        let c1 = g.mk_field("let c = &mut b.y", false, a, 1_u32);
        let bb = g.mk_field("let bb = &mut b.y", false, b1, 1_u32);
        g.mk_store_addr("*c = 2", false, c1);
        g.mk_store_addr("*bb = 1", false, bb);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = ColorPoint { x: 0, y: 0, z: Color { r: 100, g: 100, b: 100 } };
    /// let b = &mut a.x;
    /// let c = &mut a.y;
    /// a.z.r = 200;
    /// *b = 4;
    /// *c = 2;
    /// let d = &mut a;
    /// *d = ColorPoint { x: 0, y: 0, z: Color { r: 20, g: 200, b: 20 } };
    /// let e = &mut a.z;
    /// let f = &mut e.g;
    /// let g = &mut e.g;
    /// *f = 3;
    /// a.z.r = 100;
    /// ```
    #[test]
    fn lots_of_siblings() {
        let mut g = CheckedGraph::default();

        let (x, y, z) = (0_u32, 1_u32, 2_u32);
        let (red, green, _blue) = (0_u32, 1_u32, 2_u32);

        let a = g.mk_addr_of_local(
            "let mut a = ColorPoint { x: 0, y: 0, z: Color { r: 100, g: 100, b: 100 } }",
            true,
            0_u32,
        );
        let b1 = g.mk_field("let b = &mut a.x", true, a, x);
        let c1 = g.mk_field("let c = &mut a.y", true, a, y);
        let x1 = g.mk_field("a.z.r = 200", true, a, z);
        let x2 = g.mk_field("a.z.r = 200", true, x1, red);
        g.mk_store_addr("a.z.r = 200", true, x2);
        g.mk_store_addr("*b = 4", true, b1);
        g.mk_store_addr("*c = 2", true, c1);
        let d1 = g.mk_copy("let d = &mut a", true, a);
        g.mk_store_addr(
            "*d = ColorPoint { x: 0, y: 0, z: Color { r: 20, g: 200, b: 20 } }",
            true,
            d1,
        );
        let e = g.mk_field("let e = &mut a.z", false, a, z);
        let f1 = g.mk_field("let f = &mut e.g", false, e, green);
        g.mk_field("let g = &mut e.g", false, e, green);
        g.mk_store_addr("*f = 3", false, f1);
        let x4 = g.mk_field("a.z.r = 100", true, a, z);
        let x5 = g.mk_field("a.z.r = 100", true, x4, green);
        g.mk_store_addr("a.z.r = 100", true, x5);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = (1, 2);
    /// let x = &mut a.0;
    /// let y = &mut a.1;
    /// *x = 1;
    /// *y = 1;
    /// *x = 2;
    /// *y = 2;
    /// ```
    ///
    /// ```text
    /// A
    /// +------.
    /// X1     |
    /// |      Y1
    /// X2     |
    /// |      Y2
    /// X3     |
    ///        Y3
    /// ```
    #[test]
    fn field_no_conflict() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = (1, (2, 3))", true, 0_u32);
        let x1 = g.mk_field("let x = &mut a.1.0", true, a, 0_u32);
        let y1 = g.mk_field("let y = &mut a.1.1", true, a, 1_u32);
        g.mk_store_addr("*x = 1", true, x1);
        g.mk_store_addr("*y = 1", true, y1);
        g.mk_store_addr("*x = 2", true, x1);
        g.mk_store_addr("*y = 2", true, y1);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = (1, (2, 3));
    /// let x = &mut a.1.0;
    /// let y = &mut a.1.1;
    /// *x = 1;
    /// *y = 1;
    /// *x = 2;
    /// *y = 2;
    /// ```
    ///
    /// ```text
    /// A
    /// +------.
    /// X1     |
    /// X2     |
    /// |      Y1
    /// |      Y2
    /// X3     |
    /// |      Y3
    /// X4     |
    ///        Y4
    /// ```
    #[test]
    fn nested_field_no_conflict() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = (1, (2, 3))", true, 0_u32);
        let x1 = g.mk_field("let x = &mut a.1.0", true, a, 1_u32);
        let x2 = g.mk_field("let x = &mut a.1.0", true, x1, 0_u32);
        let y1 = g.mk_field("let y = &mut a.1.1", true, a, 1_u32);
        let y2 = g.mk_field("let y = &mut a.1.1", true, y1, 1_u32);
        g.mk_store_addr("*x = 1", true, x2);
        g.mk_store_addr("*y = 1", true, y2);
        g.mk_store_addr("*x = 2", true, x2);
        g.mk_store_addr("*y = 2", true, y2);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = (1, (2, 3));
    /// let x = &mut a.1.0;
    /// let y = &mut a.1.0;
    /// *x = 1;
    /// *y = 1;
    /// *x = 2;
    /// *y = 2;
    /// ```
    ///
    /// ```text
    /// A
    /// +------.
    /// X1     |
    /// X2     |
    /// |      Y1
    /// |      Y2
    /// X3     |
    /// |      Y3
    /// X4     |
    ///        Y4
    /// ```
    #[test]
    fn nested_field_conflict() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = (1, (2, 3))", false, 0_u32);
        let x1 = g.mk_field("let x = &mut a.1.0", false, a, 1_u32);
        let x2 = g.mk_field("let x = &mut a.1.0", false, x1, 0_u32);
        let y1 = g.mk_field("let y = &mut a.1.0", false, a, 1_u32);
        let y2 = g.mk_field("let y = &mut a.1.0", false, y1, 0_u32);
        g.mk_store_addr("*x = 1", false, x2);
        g.mk_store_addr("*y = 1", false, y2);
        g.mk_store_addr("*x = 2", false, x2);
        g.mk_store_addr("*y = 2", false, y2);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = ([1, 2], [3, 4]);
    /// let x = &mut a.0[0];
    /// let y = &mut a.0[1];
    /// *x = 1;
    /// *y = 1;
    /// *x = 2;
    /// *y = 2;
    /// ```
    ///
    /// ```text
    /// A
    /// +------.
    /// X1     |
    /// X2     |
    /// |      Y1
    /// |      Y2
    /// X3     |
    /// |      Y3
    /// X4     |
    ///        Y4
    /// ```
    #[test]
    fn field_offset_conflict() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = ([1, 2], [3, 4])", false, 0_u32);
        let x1 = g.mk_field("let x = &mut a.0[0]", false, a, 1_u32);
        let x2 = g.mk_offset("let x = &mut a.0[0]", false, x1, 0);
        let y1 = g.mk_field("let y = &mut a.0[1]", false, a, 1_u32);
        let y2 = g.mk_offset("let y = &mut a.0[1]", false, y1, 1);
        g.mk_store_addr("*x = 1", false, x2);
        g.mk_store_addr("*y = 1", false, y2);
        g.mk_store_addr("*x = 2", false, x2);
        g.mk_store_addr("*y = 2", false, y2);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = ([1, 2], [3, 4]);
    /// let x = &mut a.0[0];
    /// let y = &mut a.1[0];
    /// *x = 1;
    /// *y = 1;
    /// *x = 2;
    /// *y = 2;
    /// ```
    ///
    /// ```text
    /// A
    /// +------.
    /// X1     |
    /// X2     |
    /// |      Y1
    /// |      Y2
    /// X3     |
    /// |      Y3
    /// X4     |
    ///        Y4
    /// ```
    #[test]
    fn field_offset_no_conflict() {
        let mut g = CheckedGraph::default();

        let a = g.mk_addr_of_local("let mut a = ([1, 2], [3, 4])", true, 0_u32);
        let x1 = g.mk_field("let x = &mut a.0[0]", true, a, 0_u32);
        let x2 = g.mk_offset("let x = &mut a.0[0]", true, x1, 0);
        let y1 = g.mk_field("let y = &mut a.1[0]", true, a, 1_u32);
        let y2 = g.mk_offset("let y = &mut a.1[0]", true, y1, 0);
        g.mk_store_addr("*x = 1", true, x2);
        g.mk_store_addr("*y = 1", true, y2);
        g.mk_store_addr("*x = 2", true, x2);
        g.mk_store_addr("*y = 2", true, y2);

        g.check_unique();
    }

    /// ```rust
    /// let mut a = [(1, 2)];
    /// let p = &mut a;
    /// let x = &mut (*p)[0].0;
    /// let y = &mut (*p)[0].1;
    /// *x = 1;
    /// *y = 1;
    /// *x = 2;
    /// *y = 2;
    /// ```
    ///
    /// ```text
    /// A
    /// +------.
    /// X1     |
    /// X2     |
    /// |      Y1
    /// |      Y2
    /// X3     |
    /// |      Y3
    /// X4     |
    ///        Y4
    /// ```
    ///
    /// Note that this code is accepted by `rustc`,
    /// but we'd like to reject it because some of the transformations we plan to do
    /// will convert it to a form that `rustc` will reject.
    /// Specifically, if `p` were a function argument and we changed it
    /// from `&mut [_]` to something like `&mut Vec<_>` or a custom smart pointer type,
    /// `rustc` would reject the modified code.
    #[test]
    fn offset_field_conflict() {
        let mut g = CheckedGraph::default();

        let p = g.mk_addr_of_local("let mut a = ([1, 2], [3, 4]); let p = &mut a", false, 0_u32);
        let x1 = g.mk_offset("let x = &mut (*p)[0].0", false, p, 0);
        let x2 = g.mk_field("let x = &mut (*p)[0].0", false, x1, 0_u32);
        let y1 = g.mk_offset("let y = &mut (*p)[0].1", false, p, 0);
        let y2 = g.mk_field("let y = &mut (*p)[0].1", false, y1, 1_u32);
        g.mk_store_addr("*x = 1", false, x2);
        g.mk_store_addr("*y = 1", false, y2);
        g.mk_store_addr("*x = 2", false, x2);
        g.mk_store_addr("*y = 2", false, y2);

        g.check_unique();
    }
}
