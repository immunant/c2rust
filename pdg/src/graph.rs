use c2rust_analysis_rt::mir_loc::{self, DefPathHash, Func};
use c2rust_analysis_rt::mir_loc::{FuncId, MirPlace};
use rustc_index::newtype_index;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{BasicBlock, Field, Local};
use std::fmt::Display;
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};
use serde::{Serialize, Deserialize, Serializer, Deserializer};

use crate::info::NodeInfo;
use crate::util::pad_columns;
use crate::util::ShortOption;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy, Serialize, Deserialize)]
pub enum NodeKind {
    /// A copy from one [`Local`] to another.
    ///
    /// This also covers casts such as `&mut T` to `&T` or `&T` to `*const T`
    /// that don't change the type or value of the pointer.
    Copy,

    /// [`Field`] projection.
    ///
    /// Used for operations like `_2 = &(*_1).0`.
    /// Nested field accesses like `_4 = &(*_1).x.y.z`
    /// are broken into multiple [`Node`]s, each covering one level.
    Field(#[serde(with = "crate::util::serde::FieldDef")] Field),

    /// Pointer arithmetic.
    ///
    /// The [`isize`] is the concrete offset distance.
    /// We use this to detect when two pointers always refer to different indices.
    Offset(isize),

    /// Get the address of a [`Local`].
    ///
    /// For address-taken [`Local`]s, the root node is an [`AddrOfLocal`](Self::AddrOfLocal)
    /// attributed to the first [`Statement`](rustc_middle::mir::Statement) of the function.
    /// Taking the address of the [`Local`], as in `_2 = &_1`,
    /// appears as a copy of that root pointer,
    /// and reading or writing from the [`Local`]
    /// shows up as a [`LoadAddr`](Self::LoadAddr) or [`StoreAddr`](Self::StoreAddr).
    /// This allows us to track uses of the [`Local`]
    /// that interfere with an existing reference,
    /// even when those uses don't go through a pointer.
    ///
    /// Can't have a [`Node::source`].
    AddrOfLocal(#[serde(with = "crate::util::serde::LocalDef")] Local),

    /// Get the address of a static.
    ///
    /// These are treated the same as [`Local`]s,
    /// with an [`_AddressOfStatic`](Self::_AddrOfStatic) attributed to
    /// the first [`Statement`](rustc_middle::mir::Statement).
    ///
    /// Can't have a [`Node::source`].
    _AddrOfStatic(DefPathHash),

    /// Heap allocation.
    ///
    /// The [`usize`] is the number of array elements allocated
    /// For allocations of a single object, this value is 1.
    ///
    /// Can't have a [`Node::source`].
    Alloc(usize),

    /// Heap deallocation.
    ///
    /// The object described by the current [`Graph`] is no longer valid after this point.
    /// Correct programs will only [`Free`](Self::Free) pointers produced by [`Alloc`](Self::Alloc),
    /// and will no longer [`LoadAddr`](Self::LoadAddr) or [`StoreAddr`](Self::StoreAddr)
    /// any pointers derived from that [`Alloc`](Self::Alloc) afterward.
    ///
    /// Can't be the [`Node::source`] of any other operation.
    Free,

    /// Int to pointer conversion.
    ///
    /// Details TBD.
    ///
    /// Can't have a [`Node::source`].
    IntToPtr,

    /// Pointer to int conversion.
    ///
    /// Details TBD.
    ///
    /// Can't be the [`Node::source`] of any other operation.
    PtrToInt,

    /// The result of loading a value through some other pointer.
    ///
    /// Details TBD.
    ///
    /// Can't have a [`Node::source`].
    LoadValue,

    /// The pointer appears as the address of a load operation.
    ///
    /// Can't be the [`Node::source`] of any other operation.
    LoadAddr,

    /// The pointer appears as the address of a store operation.
    ///
    /// Can't be the [`Node::source`] of any other operation.
    StoreAddr,

    /// The pointer is stored through some other pointer.
    ///
    /// Details TBD.
    ///
    /// Can't be the [`Node::source`] of any other operation.
    StoreValue,
}

impl Display for NodeKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use NodeKind::*;
        match self {
            Copy => write!(f, "copy"),
            Field(field) => write!(f, "field.{}", field.as_usize()),
            Offset(offset) => write!(f, "offset[{offset}]"),
            AddrOfLocal(local) => write!(f, "&{local:?}"),
            _AddrOfStatic(static_) => write!(f, "&'static {static_:?}"),
            Alloc(n) => {
                // Right now we only create `Alloc(1)`, so special case it,
                // as the increased readability helps.
                write!(f, "alloc")?;
                if *n != 1 {
                    write!(f, "(n = {n})")?;
                }
                Ok(())
            }
            Free => write!(f, "free"),
            PtrToInt => write!(f, "ptr_to_int"),
            IntToPtr => write!(f, "int_to_ptr"),
            LoadValue => write!(f, "value.load"),
            StoreValue => write!(f, "value.store"),
            LoadAddr => write!(f, "addr.load"),
            StoreAddr => write!(f, "addr.store"),
        }
    }
}

/// A node in the graph represents an operation on pointers.  It may produce a pointer from
/// nothing, derive a pointer from another pointer, or consume a pointer without producing any
/// output.
///
/// Each operation occurs at a point in time, but the timestamp is not stored explicitly.  Instead,
/// nodes in each graph are stored in sequential order, and timing relationships can be identified
/// by comparing `NodeId`s.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Serialize, Deserialize)]
pub struct Node {
    /// The function that contains this operation.
    ///
    /// For function calls, copies from the caller's values into the callee's argument locals are
    /// attributed to the first statement of the callee, and the copy from the callee's return
    /// place to the caller's destination local is attributed to the `Call` terminator in the
    /// caller.  This way, the combination of `function` and `dest` accurately identifies the local
    /// modified by the operation.
    pub function: Func,
    /// The basic block that contains this operation.
    #[serde(with = "crate::util::serde::BasicBlockDef")]
    pub block: BasicBlock,
    /// The index within the basic block of the MIR statement or terminator that performed this
    /// operation.  As in `rustc_middle::mir::Location`, an index less than the number of
    /// statements in the block refers to that statement, and an index equal to the number of
    /// statements refers to the terminator.
    pub statement_idx: usize,
    /// The MIR place where this operation stores its result.  This is `None` for operations that
    /// don't store anything and for operations whose result is a temporary not visible as a MIR
    /// place.
    pub dest: Option<MirPlace>,
    /// The kind of operation that was performed.
    pub kind: NodeKind,
    /// The `Node` that produced the input to this operation.
    pub source: Option<NodeId>,
    /// Any string useful for debugging.
    pub debug_info: String,
    /// Information about the [`Node`] computed from the pdg.
    pub info: Option<NodeInfo>,
}

struct BlockStatement<'a> {
    block: &'a BasicBlock,
    statement_idx: &'a usize,
}

impl Display for BlockStatement<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Self {
            block: bb,
            statement_idx: stmt,
        } = self;
        write!(f, "{bb:?}[{stmt}]")
    }
}

impl Node {
    fn fmt_with_sep(&self, f: &mut Formatter, sep: char) -> fmt::Result {
        let Self {
            function,
            block,
            statement_idx,
            dest,
            kind,
            source,
            debug_info,
            info,
        } = self;
        let src = ShortOption(source.as_ref());
        let dest = ShortOption(dest.as_ref());
        let bb_stmt = BlockStatement {
            block,
            statement_idx,
        };
        let fn_ = function;
        let info = info.as_ref().map(|i| i.to_string()).unwrap_or_default();
        write!(
            f,
            "{kind}{sep}{src}{sep}=>{sep}{dest}{sep}@{sep}{bb_stmt}:{sep}fn {fn_};{sep}{info}{sep}{debug_info};"
        )
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.fmt_with_sep(f, ' ')
    }
}

newtype_index!(
    /// Implement `Idx` and other traits like MIR indices (`Local`, `BasicBlock`, etc.)
    pub struct NodeId { DEBUG_FORMAT = "NodeId({})" }
);

impl Serialize for NodeId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_u32().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for NodeId {
    fn deserialize<D>(deserializer: D) -> Result<NodeId, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = u32::deserialize(deserializer)?;
        Ok(NodeId::from_u32(raw))
    }
}

/// Implement `Idx` and other traits like MIR indices (`Local`, `BasicBlock`, etc.)
pub const _ROOT_NODE: NodeId = NodeId::from_u32(0);

impl Display for NodeId {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "n[{}]", self.as_usize())
    }
}

struct DisplayNode<'a> {
    id: NodeId,
    node: &'a Node,
    sep: char,
}

impl Display for DisplayNode<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Self { id, node, sep } = *self;
        write!(f, "{id}:{sep}")?;
        node.fmt_with_sep(f, sep)?;
        Ok(())
    }
}

/// A pointer derivation graph, which tracks the handling of one object throughout its lifetime.
#[derive(Debug, Default, Eq, PartialEq, Hash, Clone, Serialize, Deserialize)]
pub struct Graph {
    /// The nodes in the graph.  Nodes are stored in increasing order by timestamp.  The first
    /// node, called the "root node", creates the object described by this graph, and all other
    /// nodes are derived from it.
    #[serde(with = "crate::util::serde::index_vec")]
    pub nodes: IndexVec<NodeId, Node>,
}

impl Graph {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Display for Graph {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let sep = '|';
        let lines = self
            .nodes
            .iter_enumerated()
            .map(|(node_id, node)| {
                DisplayNode {
                    id: node_id,
                    node,
                    sep,
                }
                .to_string()
            })
            .collect::<Vec<_>>();
        writeln!(f, "g {{")?;
        for line in pad_columns(&lines, sep, " ") {
            let line = line.trim_end();
            writeln!(f, "\t{line}")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

newtype_index!(
    /// Implement `Idx` and other traits like MIR indices (`Local`, `BasicBlock`, etc.)
    pub struct GraphId { DEBUG_FORMAT = "GraphId({})" }
);

impl Serialize for GraphId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_u32().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for GraphId {
    fn deserialize<D>(deserializer: D) -> Result<GraphId, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = u32::deserialize(deserializer)?;
        Ok(GraphId::from_u32(raw))
    }
}

impl Display for GraphId {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "g[{}]", self.as_usize())
    }
}

/// A collection of graphs describing the handling of one or more objects within the program.
#[derive(Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct Graphs {
    /// The graphs.  Each graph describes one object, or one group of objects that were all handled
    /// identically.
    #[serde(with = "crate::util::serde::index_vec")]
    pub graphs: IndexVec<GraphId, Graph>,

    /// Lookup table for finding all nodes in all graphs that store to a particular MIR local.
    pub latest_assignment: HashMap<(FuncId, mir_loc::Local), (GraphId, NodeId)>,
}

impl Graphs {
    pub fn new() -> Self {
        Self::default()
    }

    /// The [`Node::dest`] node of [`AddrOfLocal`]is always `Some(local)`
    /// and is used in determining the sources of subsequent PDG nodes.
    /// However, for the purposes of static analysis, it's undesired
    /// and therefore destinations of [`AddrOfLocal`] [`Node`]s are removed.
    ///
    /// [`AddrOfLocal`]: NodeKind::AddrOfLocal
    /// [`Node`]: crate::graph::Node
    /// [`Node::dest`]: crate::graph::Node::dest
    pub fn remove_addr_of_local_sources(&mut self) {
        for graph in &mut self.graphs {
            for node in &mut graph.nodes {
                if let NodeKind::AddrOfLocal(..) = node.kind {
                    node.dest = None;
                }
            }
        }
    }
}

impl Display for Graphs {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for (i, graph) in self.graphs.iter().enumerate() {
            if i != 0 {
                write!(f, "\n\n")?;
            }
            write!(f, "{graph}")?;
        }
        Ok(())
    }
}
