use crate::graph::{Graph, Graphs, Node, NodeId, NodeKind};
use rustc_middle::mir::Field;
use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug, Display, Formatter};

/// Force an import of [`Node`] just for docs.
const _: Option<Node> = None;

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
    fn new(n_id: NodeId, k: NodeKind) -> FlowInfo {
        use NodeKind::*;
        FlowInfo {
            load: matches!(k, LoadAddr | LoadValue).then(|| n_id),
            store: matches!(k, StoreAddr | StoreValue).then(|| n_id),
            pos_offset: matches!(k, Offset(x) if x > 0).then(|| n_id),
            neg_offset: matches!(k, Offset(x) if x < 0).then(|| n_id),
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
            .map(|(idx, node)| (idx, FlowInfo::new(idx, node.kind))),
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

/// Maps each [`Node`] in a [`Graph`] to its chronologically (according to [`NodeId`]) final descendant.
///
/// The [`Graph`] is assumed to be acyclic and topologically sorted, but not necessarily connected.
fn get_last_desc(g: &mut Graph) -> HashMap<NodeId, NodeId> {
    let mut desc_map = g
        .nodes
        .indices()
        .map(|idx| (idx, idx))
        .collect::<HashMap<_, _>>();
    for (child, parent) in g
        .nodes
        .iter_enumerated()
        .rev()
        .filter_map(|(child, child_node)| Some((child, child_node.source?)))
    {
        let child = desc_map[&child];
        desc_map
            .entry(parent)
            .and_modify(|parent| *parent = max(*parent, child));
    }
    desc_map
}

/// Finds the inverse of a [`Graph`], each [`Node`] mapping to a [`Vec`] of its immediate
/// descendents and their relationship to the [`Node`] in terms of fields.
fn collect_children(g: &Graph) -> HashMap<NodeId, Vec<(NodeId, Vec<Field>)>> {
    let mut children = HashMap::<_, Vec<(NodeId, Vec<Field>)>>::new();
    for parent in g.nodes.indices() {
        children.entry(parent).or_default();
    }
    for (parent, child, child_node) in g
        .nodes
        .iter_enumerated()
        .rev()
        .filter_map(|(child, child_node)| Some((child_node.source?, child, child_node)))
    {
        if let NodeKind::Field(f) = child_node.kind {
            let my_children =
                children
                    .remove(&child)
                    .unwrap()
                    .into_iter()
                    .map(|(gchild, mut gchildf)| {
                        (
                            gchild,
                            ({
                                gchildf.push(f);
                                gchildf
                            }),
                        )
                    });
            children.insert(child, Vec::new());
            children.get_mut(&parent).unwrap().extend(my_children);
        } else {
            children
                .get_mut(&parent)
                .unwrap()
                .push((child, Vec::<_>::new()));
        }
    }
    children
}

/// Checks if fs1 and fs2 are either prefixes of each other, which would make them aliases.
/// Note: the fields were pushed on from the bottom up, so they are in reverse order.
fn prefix(fs1: &Vec<Field>, fs2: &Vec<Field>) -> bool {
    let len1 = fs1.len();
    let len2 = fs2.len();
    for i in 0..min(len1, len2) {
        if fs1[len1 - 1 - i] != fs2[len2 - 1 - i] {
            return false;
        }
    }
    true
}

/// Given a list of [`Node`]s of the same parent and information about them,
/// determines if any have conflicts with any of the others.
/// Children which are not a field cannot be live at the same time as any other child.
/// Children which are a field cannot be live at the same time as any other one of the same field.
fn check_children_conflict(
    parent: &NodeId,
    children: &HashMap<NodeId, Vec<(NodeId, Vec<Field>)>>,
    descs: &HashMap<NodeId, NodeId>,
) -> bool {
    let mut max_descs = HashMap::<Vec<Field>, NodeId>::new();
    let mut node_children = children[parent].clone();
    node_children.sort_by(|(id1, _), (id2, _)| id1.cmp(id2));
    for (child, child_fields) in node_children {
        let conflicts =
            |fields| matches!(max_descs.get(fields), Some(max_desc) if max_desc > &child);
        if max_descs
            .keys()
            .filter(|sib_fields| prefix(sib_fields, &child_fields))
            .any(conflicts)
        {
            return true;
        }
        let cur = descs[&child];
        max_descs
            .entry(child_fields.clone())
            .and_modify(|past| *past = max(*past, cur))
            .or_insert(cur);
    }
    false
}

/// Compute and set [`NodeInfo::unique`].
///
/// If a [`Node`] is not [`unique`], none of its descendents can be [`unique`].
/// If any of a node's children conflict with each other, it is not [`unique`].
/// Because we traverse the [`Graph`] visiting all parents before their children,
/// just checking the immediate parent's [`unique`]ness status
/// is sufficient to guarantee the first condition.
///
/// [`unique`]: NodeInfo::unique
fn set_uniqueness(g: &mut Graph) {
    let children = collect_children(g);
    let last_descs = get_last_desc(g);
    let mut non_uniqueness = HashSet::new();
    for (child, child_node) in g.nodes.iter_enumerated() {
        let parent = child_node.source;
        if matches!(parent, Some(parent) if non_uniqueness.contains(&parent))
            || check_children_conflict(&child, &children, &last_descs)
        {
            non_uniqueness.insert(child);
        }
    }
    for (n_id, node) in g.nodes.iter_enumerated_mut() {
        node.info.as_mut().unwrap().unique = !non_uniqueness.contains(&n_id);
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
    use c2rust_analysis_rt::mir_loc::FuncId;
    use rustc_middle::mir::Field;
    use rustc_middle::mir::Local;

    fn mk_node(g: &mut Graph, kind: NodeKind, source: Option<NodeId>) -> NodeId {
        g.nodes.push(Node {
            function: Func {
                id: FuncId((1, 2).into()),
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

    fn mk_offset(g: &mut Graph, source: NodeId, i: isize) -> NodeId {
        mk_node(g, NodeKind::Offset(i), Some(source))
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
        let mut g = Graph::default();

        // let mut a = 0;
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let b = &mut a;
        let b1 = mk_copy(&mut g, a);
        // *b = 0;
        let b2 = mk_store_addr(&mut g, b1);
        // let c = &mut a;
        let c1 = mk_copy(&mut g, a);
        // *c = 0;
        let c2 = mk_store_addr(&mut g, c1);
        // *b = 0;
        let b3 = mk_store_addr(&mut g, b1);
        // *c = 0;
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
        let mut g = Graph::default();

        // let mut a = 0;   // A
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let b = &mut a;  // B1
        let b1 = mk_copy(&mut g, a);
        // *b = 0;          // B2
        let b2 = mk_store_addr(&mut g, b1);
        // let c = &mut a;  // C1
        let c1 = mk_copy(&mut g, a);
        // *c = 0;          // C2
        let c2 = mk_store_addr(&mut g, c1);
        // *b = 0;          // B3
        let b3 = mk_store_addr(&mut g, b1);

        let pdg = build_pdg(g);
        assert!(!info(&pdg, a).unique);
        assert!(!info(&pdg, b1).unique);
        assert!(!info(&pdg, b2).unique);
        assert!(!info(&pdg, b3).unique);
        assert!(!info(&pdg, c1).unique);
        assert!(!info(&pdg, c2).unique);
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
        let mut g = Graph::default();

        // let mut a = 0;
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let b = &mut a;
        let b1 = mk_copy(&mut g, a);
        // *b = 0;
        let b2 = mk_store_addr(&mut g, b1);
        // let c = &mut *b;
        let c1 = mk_copy(&mut g, b1);
        // *c = 0;
        let c2 = mk_store_addr(&mut g, c1);
        // *c = 0;
        let b3 = mk_store_addr(&mut g, b1);

        let pdg = build_pdg(g);
        assert!(info(&pdg, a).unique);
        assert!(info(&pdg, b1).unique);
        assert!(info(&pdg, b2).unique);
        assert!(info(&pdg, b3).unique);
        assert!(info(&pdg, c1).unique);
        assert!(info(&pdg, c2).unique);
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
        let mut g = Graph::default();

        // let mut a = 0;
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let b = &mut a;
        let b1 = mk_copy(&mut g, a);
        // *b = 0;
        let b2 = mk_store_addr(&mut g, b1);
        // let c = &mut *b;
        let c1 = mk_copy(&mut g, b1);
        // *c = 0;
        let c2 = mk_store_addr(&mut g, c1);
        // *b = 0;
        let b3 = mk_store_addr(&mut g, b1);
        // *c = 0;
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
        let mut g = Graph::default();

        // let mut a = Point { x: 0, y: 0 };
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let b = &mut a.x;
        let b11 = mk_field(&mut g, a, 0_u32);
        let b1 = mk_copy(&mut g, b11);
        // let c = &mut a.y;
        let c11 = mk_field(&mut g, a, 1_u32);
        let c1 = mk_copy(&mut g, c11);
        // *b = 1;
        let b2 = mk_store_addr(&mut g, b1);
        // *c = 2;
        let c2 = mk_store_addr(&mut g, c1);

        let pdg = build_pdg(g);
        assert!(info(&pdg, a).unique);
        assert!(info(&pdg, b1).unique);
        assert!(info(&pdg, b2).unique);
        assert!(info(&pdg, c1).unique);
        assert!(info(&pdg, c2).unique);
    }

    /// ```rust
    /// let mut a = Point { x: 0, y: 0 };
    /// let j = &mut a;
    /// let b = &mut j.x;
    /// let c = &mut j.x;
    /// *b = 1;
    /// *c = 2;
    /// a.y = 3;
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
        let mut g = Graph::default();

        // let mut a = Point { x: 0, y: 0 };
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let j = &mut a;
        let j = mk_copy(&mut g, a);
        // let b = &mut j.x;
        let b11 = mk_field(&mut g, j, 0_u32);
        let b1 = mk_copy(&mut g, b11);
        // let c = &mut j.x;
        let c11 = mk_field(&mut g, j, 0_u32);
        let c1 = mk_copy(&mut g, c11);
        // *b = 1;
        let b2 = mk_store_addr(&mut g, b1);
        // *c = 2;
        let c2 = mk_store_addr(&mut g, c1);
        // *(a.y) = 3;
        let d1 = mk_field(&mut g, a, 1_u32);
        let d2 = mk_store_addr(&mut g, d1);

        let pdg = build_pdg(g);
        assert!(info(&pdg, a).unique);
        assert!(!info(&pdg, j).unique);
        assert!(!info(&pdg, b1).unique);
        assert!(!info(&pdg, b2).unique);
        assert!(!info(&pdg, c1).unique);
        assert!(!info(&pdg, c2).unique);
        assert!(info(&pdg, d2).unique);
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
        let mut g = Graph::default();

        // let mut a = Point { x: 0, y: 0 };
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let b = &mut a;
        let b1 = mk_copy(&mut g, a);
        // let c = &mut a.y;
        let c11 = mk_field(&mut g, a, 1_u32);
        let c1 = mk_copy(&mut g, c11);
        // *c = 2;
        let c2 = mk_store_addr(&mut g, c1);
        // *b = 1;
        let b2 = mk_store_addr(&mut g, b1);

        let pdg = build_pdg(g);
        assert!(!info(&pdg, a).unique);
        assert!(!info(&pdg, b1).unique);
        assert!(!info(&pdg, b2).unique);
        assert!(!info(&pdg, c1).unique);
        assert!(!info(&pdg, c2).unique);
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
    /// |      C2
    /// |y     |
    /// BB     |
    /// BB1    |
    /// |      C3
    /// B2
    /// ```
    #[test]
    fn fields_different_levels() {
        let mut g = Graph::default();

        // let mut a = Point { x: 0, y: 0 };
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let b = &mut a;
        let b1 = mk_copy(&mut g, a);
        // let c = &mut b.y;
        let c1 = mk_field(&mut g, a, 1_u32);
        let c2 = mk_copy(&mut g, c1);
        // let bb = &mut b.y;
        let bb = mk_field(&mut g, b1, 1_u32);
        let bb1 = mk_copy(&mut g, bb);
        // *c = 2;
        let c3 = mk_store_addr(&mut g, c2);
        // *bb = 1;
        let b2 = mk_store_addr(&mut g, bb1);

        let pdg = build_pdg(g);
        assert!(!info(&pdg, a).unique);
        assert!(!info(&pdg, b1).unique);
        assert!(!info(&pdg, b2).unique);
        assert!(!info(&pdg, c1).unique);
        assert!(!info(&pdg, c2).unique);
        assert!(!info(&pdg, c3).unique);
        assert!(!info(&pdg, bb1).unique);
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
        let mut g = Graph::default();

        let (x, y, z) = (0_u32, 1_u32, 2_u32);
        let (red, green, _blue) = (0_u32, 1_u32, 2_u32);

        // let mut a = ColorPoint { x: 0, y: 0, z: Color { r: 100, g: 100, b: 100 } };
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let b = &mut a.x;
        let bb1 = mk_field(&mut g, a, x);
        let b1 = mk_copy(&mut g, bb1);
        // let c = &mut a.y;
        let cc1 = mk_field(&mut g, a, y);
        let c1 = mk_copy(&mut g, cc1);
        // a.z.r = 200;
        let x1 = mk_field(&mut g, a, z);
        let x2 = mk_field(&mut g, x1, red);
        let x3 = mk_store_addr(&mut g, x2);
        // *b = 4;
        let b2 = mk_store_addr(&mut g, b1);
        // *c = 2;
        let c2 = mk_store_addr(&mut g, c1);
        // let d = &mut a;
        let d1 = mk_copy(&mut g, a);
        // *d = ColorPoint { x: 0, y: 0, z: Color { r: 20, g: 200, b: 20 } };
        let d2 = mk_store_addr(&mut g, d1);
        // let e = &mut a.z;
        let ee = mk_field(&mut g, a, z);
        let e = mk_copy(&mut g, ee);
        // let f = &mut e.g;
        let ff1 = mk_field(&mut g, e, green);
        let f1 = mk_copy(&mut g, ff1);
        // let g = &mut e.g;
        let ggg = mk_field(&mut g, e, green);
        let gg = mk_copy(&mut g, ggg);
        // *f = 3;
        let f2 = mk_store_addr(&mut g, f1);
        // a.z.r = 100;
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
    /// X2     |
    /// |      Y1
    /// |      Y2
    /// X3     |
    /// |      Y3
    /// X4     |
    ///        Y4
    /// ```
    #[test]
    fn field_no_conflict() {
        let mut g = Graph::default();

        // let mut a = (1, (2, 3));
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let x = &mut a.0;
        let x1 = mk_field(&mut g, a, 0_u32);
        let x2 = mk_copy(&mut g, x1);
        // let y = &mut a.1;
        let y1 = mk_field(&mut g, a, 1_u32);
        let y2 = mk_copy(&mut g, y1);
        // *x = 1;
        let x3 = mk_store_addr(&mut g, x2);
        // *y = 1;
        let y3 = mk_store_addr(&mut g, y2);
        // *x = 2;
        let x4 = mk_store_addr(&mut g, x3);
        // *y = 2;
        let y4 = mk_store_addr(&mut g, y3);

        let pdg = build_pdg(g);

        assert!(info(&pdg, a).unique);
        assert!(info(&pdg, x1).unique);
        assert!(info(&pdg, x2).unique);
        assert!(info(&pdg, x3).unique);
        assert!(info(&pdg, x4).unique);
        assert!(info(&pdg, y1).unique);
        assert!(info(&pdg, y2).unique);
        assert!(info(&pdg, y3).unique);
        assert!(info(&pdg, y4).unique);
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
    /// X3     |
    /// |      Y1
    /// |      Y2
    /// |      Y3
    /// X4     |
    /// |      Y4
    /// X5     |
    ///        Y5
    /// ```
    #[test]
    fn nested_field_no_conflict() {
        let mut g = Graph::default();

        // let mut a = (1, (2, 3));
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let x = &mut a.1.0;
        let x1 = mk_field(&mut g, a, 1_u32);
        let x2 = mk_field(&mut g, x1, 0_u32);
        let x3 = mk_copy(&mut g, x2);
        // let y = &mut a.1.1;
        let y1 = mk_field(&mut g, a, 1_u32);
        let y2 = mk_field(&mut g, y1, 1_u32);
        let y3 = mk_copy(&mut g, y2);
        // *x = 1;
        let x4 = mk_store_addr(&mut g, x3);
        // *y = 1;
        let y4 = mk_store_addr(&mut g, y3);
        // *x = 2;
        let x5 = mk_store_addr(&mut g, x4);
        // *y = 2;
        let y5 = mk_store_addr(&mut g, y4);

        let pdg = build_pdg(g);

        assert!(info(&pdg, a).unique);
        assert!(info(&pdg, x1).unique);
        assert!(info(&pdg, x2).unique);
        assert!(info(&pdg, x3).unique);
        assert!(info(&pdg, x4).unique);
        assert!(info(&pdg, x5).unique);
        assert!(info(&pdg, y1).unique);
        assert!(info(&pdg, y2).unique);
        assert!(info(&pdg, y3).unique);
        assert!(info(&pdg, y4).unique);
        assert!(info(&pdg, y5).unique);
    }

    /// ```rust
    /// let mut a = (1, (2, 3));
    /// let mut x = &mut a.1;
    /// let mut y = &mut a.1;
    /// *(x.0) = 4;
    /// *(y.1) = 2;
    /// ```
    ///
    /// ```text
    /// A--------
    /// |       |
    /// X1(f0)  Y1(f0)
    /// X2      Y2
    /// X3(f0)  Y3(f1)
    /// X4      Y4
    /// ```
    #[test]
    fn diff_field_conflict() {
        let mut g = Graph::default();

        //let mut a = (1, (2, 3));
        let a = mk_addr_of_local(&mut g, 0_u32);
        //let mut x = &mut a.1;
        let x1 = mk_field(&mut g, a, 1_u32);
        let x2 = mk_copy(&mut g, x1);
        //let mut y = &mut a.1;
        let y1 = mk_field(&mut g, a, 1_u32);
        let y2 = mk_copy(&mut g, y1);
        // *(x.0) = 4;
        let x3 = mk_field(&mut g, x2, 0_u32);
        let x4 = mk_store_addr(&mut g, x3);
        // *(y.1) = 2;
        let y3 = mk_field(&mut g, y2, 1_u32);
        let y4 = mk_store_addr(&mut g, y3);

        let pdg = build_pdg(g);

        assert!(!info(&pdg, a).unique);
        assert!(!info(&pdg, x1).unique);
        assert!(!info(&pdg, x2).unique);
        assert!(!info(&pdg, x3).unique);
        assert!(!info(&pdg, x4).unique);
        assert!(!info(&pdg, y1).unique);
        assert!(!info(&pdg, y2).unique);
        assert!(!info(&pdg, y3).unique);
        assert!(!info(&pdg, y4).unique);
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
        let mut g = Graph::default();

        // let mut a = (1, (2, 3));
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let x = &mut a.1.0;
        let x1 = mk_field(&mut g, a, 1_u32);
        let x2 = mk_field(&mut g, x1, 0_u32);
        let x3 = mk_copy(&mut g, x2);
        // let y = &mut a.1.0;
        let y1 = mk_field(&mut g, a, 1_u32);
        let y2 = mk_field(&mut g, y1, 0_u32);
        let y3 = mk_copy(&mut g, y2);
        // *x = 1;
        let x4 = mk_store_addr(&mut g, x3);
        // *y = 1;
        let y4 = mk_store_addr(&mut g, y3);
        // *x = 2;
        let x5 = mk_store_addr(&mut g, x4);
        // *y = 2;
        let y5 = mk_store_addr(&mut g, y4);

        let pdg = build_pdg(g);

        assert!(!info(&pdg, a).unique);
        assert!(!info(&pdg, x1).unique);
        assert!(!info(&pdg, x2).unique);
        assert!(!info(&pdg, x3).unique);
        assert!(!info(&pdg, x4).unique);
        assert!(!info(&pdg, x5).unique);
        assert!(!info(&pdg, y1).unique);
        assert!(!info(&pdg, y2).unique);
        assert!(!info(&pdg, y3).unique);
        assert!(!info(&pdg, y4).unique);
        assert!(!info(&pdg, y5).unique);
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
    /// X3     |
    /// |      Y1
    /// |      Y2
    /// |      Y3
    /// X4     |
    /// |      Y4
    /// X5     |
    ///        Y5
    /// ```
    #[test]
    fn field_offset_conflict() {
        let mut g = Graph::default();

        // let mut a = ([1, 2], [3, 4]);
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let x = &mut a.0[0];
        let x1 = mk_field(&mut g, a, 1_u32);
        let x2 = mk_offset(&mut g, x1, 0);
        let x3 = mk_copy(&mut g, x2);
        // let y = &mut a.0[1];
        let y1 = mk_field(&mut g, a, 1_u32);
        let y2 = mk_offset(&mut g, y1, 1);
        let y3 = mk_copy(&mut g, y2);
        // *x = 1;
        let x4 = mk_store_addr(&mut g, x3);
        // *y = 1;
        let y4 = mk_store_addr(&mut g, y3);
        // *x = 2;
        let x5 = mk_store_addr(&mut g, x4);
        // *y = 2;
        let y5 = mk_store_addr(&mut g, y4);

        let pdg = build_pdg(g);

        assert!(!info(&pdg, a).unique);
        assert!(!info(&pdg, x1).unique);
        assert!(!info(&pdg, x2).unique);
        assert!(!info(&pdg, x3).unique);
        assert!(!info(&pdg, x4).unique);
        assert!(!info(&pdg, x5).unique);
        assert!(!info(&pdg, y1).unique);
        assert!(!info(&pdg, y2).unique);
        assert!(!info(&pdg, y3).unique);
        assert!(!info(&pdg, y4).unique);
        assert!(!info(&pdg, y5).unique);
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
    /// X3     |
    /// |      Y1
    /// |      Y2
    /// |      Y3
    /// X4     |
    /// |      Y4
    /// X5     |
    ///        Y5
    /// ```
    #[test]
    fn field_offset_no_conflict() {
        let mut g = Graph::default();

        // let mut a = ([1, 2], [3, 4]);
        let a = mk_addr_of_local(&mut g, 0_u32);
        // let x = &mut a.0[0];
        let x1 = mk_field(&mut g, a, 0_u32);
        let x2 = mk_offset(&mut g, x1, 0);
        let x3 = mk_copy(&mut g, x2);
        // let y = &mut a.1[0];
        let y1 = mk_field(&mut g, a, 1_u32);
        let y2 = mk_offset(&mut g, y1, 0);
        let y3 = mk_copy(&mut g, y2);
        // *x = 1;
        let x4 = mk_store_addr(&mut g, x3);
        // *y = 1;
        let y4 = mk_store_addr(&mut g, y3);
        // *x = 2;
        let x5 = mk_store_addr(&mut g, x4);
        // *y = 2;
        let y5 = mk_store_addr(&mut g, y4);

        let pdg = build_pdg(g);

        assert!(info(&pdg, a).unique);
        assert!(info(&pdg, x1).unique);
        assert!(info(&pdg, x2).unique);
        assert!(info(&pdg, x3).unique);
        assert!(info(&pdg, x4).unique);
        assert!(info(&pdg, x5).unique);
        assert!(info(&pdg, y1).unique);
        assert!(info(&pdg, y2).unique);
        assert!(info(&pdg, y3).unique);
        assert!(info(&pdg, y4).unique);
        assert!(info(&pdg, y5).unique);
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
    /// X3     |
    /// |      Y1
    /// |      Y2
    /// |      Y3
    /// X4     |
    /// |      Y4
    /// X5     |
    ///        Y5
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
        let mut g = Graph::default();

        // let mut a = ([1, 2], [3, 4]);
        // let p = &mut a;
        let p = mk_addr_of_local(&mut g, 0_u32);
        // let x = &mut (*p)[0].0;
        let x1 = mk_offset(&mut g, p, 0);
        let x2 = mk_field(&mut g, x1, 0_u32);
        let x3 = mk_copy(&mut g, x2);
        // let y = &mut (*p)[0].1;
        let y1 = mk_offset(&mut g, p, 0);
        let y2 = mk_field(&mut g, y1, 1_u32);
        let y3 = mk_copy(&mut g, y2);
        // *x = 1;
        let x4 = mk_store_addr(&mut g, x3);
        // *y = 1;
        let y4 = mk_store_addr(&mut g, y3);
        // *x = 2;
        let x5 = mk_store_addr(&mut g, x4);
        // *y = 2;
        let y5 = mk_store_addr(&mut g, y4);

        let pdg = build_pdg(g);

        assert!(!info(&pdg, p).unique);
        assert!(!info(&pdg, x1).unique);
        assert!(!info(&pdg, x2).unique);
        assert!(!info(&pdg, x3).unique);
        assert!(!info(&pdg, x4).unique);
        assert!(!info(&pdg, x5).unique);
        assert!(!info(&pdg, y1).unique);
        assert!(!info(&pdg, y2).unique);
        assert!(!info(&pdg, y3).unique);
        assert!(!info(&pdg, y4).unique);
        assert!(!info(&pdg, y5).unique);
    }
}
