use crate::graph::{Graph, GraphId, Graphs, Node, NodeId, NodeKind};
use c2rust_analysis_rt::events::{Event, EventKind, Pointer};
use c2rust_analysis_rt::metadata::Metadata;
use c2rust_analysis_rt::mir_loc::{EventMetadata, Func, FuncId, Local, MirLoc, TransferKind};
use color_eyre::eyre;
use fs_err::File;
use indexmap::IndexSet;
use itertools::Itertools;
use std::collections::BTreeMap;
use std::io::{self, BufReader};
use std::iter;
use std::path::Path;

pub fn read_event_log(path: &Path) -> io::Result<Vec<Event>> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let events = iter::from_fn(|| bincode::deserialize_from(&mut reader).ok()).collect::<Vec<_>>();
    Ok(events)
}

pub fn read_metadata(path: &Path) -> eyre::Result<Metadata> {
    let bytes = fs_err::read(path)?;
    Ok(Metadata::read(&bytes)?)
}

fn parent<'a, 'b>(e: &'a NodeKind, obj: &'b ProvenanceInfo) -> Option<&'b ProvenanceInfo> {
    use NodeKind::*;
    match e {
        Alloc(..) | AddrOfLocal(..) | AddrOfSized(..) => None,
        _ => Some(obj),
    }
}

type AddressTaken = IndexSet<(FuncId, Local)>;

pub trait EventKindExt {
    fn ptr(&self, metadata: &EventMetadata) -> Option<Pointer>;
    fn to_node_kind(
        &self,
        func: FuncId,
        metadata: &Metadata,
        address_taken: &mut AddressTaken,
    ) -> Option<NodeKind>;
}

impl EventKindExt for EventKind {
    /// return the ptr of interest for a particular event
    fn ptr(&self, _metadata: &EventMetadata) -> Option<Pointer> {
        use EventKind::*;
        Some(match *self {
            CopyPtr(lhs) => lhs,
            Project(ptr, ..) => ptr,
            Free { ptr } => ptr,
            Ret(ptr) => ptr,
            LoadAddr(ptr) => ptr,
            StoreAddr(ptr) => ptr,
            StoreAddrTaken(ptr) => ptr,
            LoadValue(ptr) => ptr,
            StoreValue(ptr) => ptr,
            ToInt(ptr) => ptr,
            Realloc { old_ptr, .. } => old_ptr,
            FromInt(lhs) => lhs,
            Alloc { ptr, .. } => ptr,
            AddrOfLocal { ptr, .. } => ptr,
            AddrOfSized { ptr, .. } => ptr,
            Offset(ptr, _, _) => ptr,
            Done | BeginFuncBody => return None,
        })
    }

    fn to_node_kind(
        &self,
        func: FuncId,
        metadata: &Metadata,
        address_taken: &mut AddressTaken,
    ) -> Option<NodeKind> {
        use EventKind::*;
        Some(match *self {
            Alloc { .. } => NodeKind::Alloc(1),
            Realloc { .. } => NodeKind::Alloc(1),
            Free { .. } => NodeKind::Free,
            CopyPtr(..) => NodeKind::Copy,
            Project(base_ptr, new_ptr, key) => {
                let proj = metadata
                    .projections
                    .get(&key)
                    .expect("Invalid projection metadata");
                NodeKind::Project(new_ptr - base_ptr, proj.clone())
            }
            LoadAddr(..) => NodeKind::LoadAddr,
            StoreAddr(..) => NodeKind::StoreAddr,
            StoreAddrTaken(..) => NodeKind::StoreAddr,
            LoadValue(..) => NodeKind::LoadValue,
            StoreValue(..) => NodeKind::StoreValue,
            AddrOfLocal { local, .. } => {
                // All but the first instance of AddrOfLocal in a given
                // function body are considered copies of that local's address
                let (_, inserted) = address_taken.insert_full((func, local));
                if inserted {
                    NodeKind::AddrOfLocal(local.as_u32().into())
                } else {
                    NodeKind::Copy
                }
            }
            AddrOfSized { size, .. } => NodeKind::AddrOfSized(size),
            BeginFuncBody => {
                // Reset the collection of address-taken locals, in order to
                // properly consider the first instance of each address-taking
                // event as that, and not as a copy.
                address_taken.clear();
                return None;
            }
            ToInt(_) => NodeKind::PtrToInt,
            FromInt(_) => NodeKind::IntToPtr,
            Ret(_) => return None,
            Offset(_, offset, _) => NodeKind::Offset(offset),
            Done => return None,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ProvenanceInfo {
    /// Size of this allocation or local object.
    size: Option<usize>,

    /// Graph containing all the nodes for this object.
    gid: GraphId,

    /// One node that created this object.
    nid: NodeId,
}

impl ProvenanceInfo {
    fn new(gid: GraphId, nid: NodeId) -> Self {
        Self {
            size: None,
            gid,
            nid,
        }
    }

    /// Return a copy of the current object with a different node.
    fn with_node(&self, nid: NodeId) -> Self {
        Self {
            nid,
            ..self.clone()
        }
    }
}

fn update_provenance(
    provenances: &mut BTreeMap<Pointer, ProvenanceInfo>,
    event_kind: &EventKind,
    mut mapping: ProvenanceInfo,
) {
    use EventKind::*;
    match *event_kind {
        Alloc { ptr, size, .. } => {
            // TODO: check for overlap with existing allocations
            mapping.size = Some(size);
            provenances.insert(ptr, mapping);
        }
        Realloc { new_ptr, size, .. } => {
            mapping.size = Some(size);
            provenances.insert(new_ptr, mapping);
        }
        Free { ptr } if ptr != 0 => {
            // TODO: handle the case where `ptr` falls inside an allocation
            let old_prov = provenances.remove(&ptr);
            if old_prov.is_none() {
                log::warn!("Tried to free invalid pointer 0x{:x}", ptr);
            }
        }
        CopyPtr(ptr) | Offset(_, _, ptr) | Project(_, ptr, _) => {
            // Check that the pointer falls inside an existing allocation
            let need_insert = provenances
                .range(0..=ptr)
                .next_back()
                .map(|(start, pi)| {
                    // If the existing pointer has a size, check it
                    // Otherwise, insert ours if it's different
                    let size = match pi.size {
                        Some(size) => size,
                        None => return ptr != *start,
                    };

                    let end = start.saturating_add(size);
                    if (*start..=end).contains(&ptr) {
                        return false;
                    }

                    if matches!(*event_kind, CopyPtr(_)) {
                        // A new pointer that we don't know where it came from
                        // Insert it for now, we should decide later what to do
                        //
                        // Assume an unknown pointer points to a word-sized object
                        log::warn!("Pointer of unknown origin 0x{:x}", ptr);
                        return true;
                    }

                    log::warn!(
                        "0x{:x} outside of object bounds 0x{:x}-0x{:x}",
                        ptr,
                        start,
                        end
                    );

                    false
                })
                .unwrap_or(true);

            if need_insert {
                provenances.insert(ptr, mapping);
            }
        }
        AddrOfLocal { ptr, size, .. } => {
            // TODO: is this a local from another function?
            mapping.size = size.try_into().ok();
            provenances.insert(ptr, mapping);
        }
        AddrOfSized { ptr, size } => {
            mapping.size = Some(size);
            let _ = provenances.try_insert(ptr, mapping);
        }
        _ => {}
    }
}

pub fn add_node(
    graphs: &mut Graphs,
    provenances: &mut BTreeMap<Pointer, ProvenanceInfo>,
    address_taken: &mut AddressTaken,
    event: &Event,
    metadata: &Metadata,
) -> Option<NodeId> {
    let MirLoc {
        func,
        mut basic_block_idx,
        mut statement_idx,
        metadata: event_metadata,
    } = metadata.get(event.mir_loc);

    let node_kind = event.kind.to_node_kind(func.id, metadata, address_taken)?;
    let this_id = func.id;
    let (_src_fn, dest_fn) = match event_metadata.transfer_kind {
        TransferKind::None => (this_id, this_id),
        TransferKind::Arg(id) => (this_id, id),
        TransferKind::Ret(id) => (id, this_id),
    };

    if let TransferKind::Arg(_) = event_metadata.transfer_kind {
        // FIXME: this is a special case for arguments
        basic_block_idx = 0;
        statement_idx = 0;
    }

    let ptr = event.kind.ptr(event_metadata);
    let provenance = ptr.and_then(|ptr| {
        provenances
            .range(0..=ptr)
            .next_back()
            .and_then(|(start, pi)| {
                let size = match pi.size {
                    Some(size) => size,
                    None if ptr == *start => return Some(pi),
                    None => return None,
                };

                // If the current pointer falls outside the
                // original object, make a new graph for it
                let end = start.saturating_add(size);

                // This is intentionally greater-than because
                // one past the end is still a valid pointer
                if ptr > end {
                    None
                } else {
                    Some(pi)
                }
            })
    });

    let direct_source = provenance.and_then(|pi| {
        graphs.graphs[pi.gid]
            .nodes
            .iter()
            .rposition(|n| {
                if let (Some(d), Some(s)) = (&n.dest, &event_metadata.source) {
                    // TODO: Ignore direct assignments with projections for now,
                    // e.g., `_1.0 = _2;`. We should later add support for
                    // assignments to sub-fields, e.g.
                    // ```
                    //   _1 = _2;
                    //   _1.0 = _3;
                    //   _1 = _4;
                    // ```
                    d == s && s.projection.is_empty()
                } else {
                    false
                }
            })
            .map(|nid| pi.with_node(NodeId::from(nid)))
    });
    let source = direct_source.or_else(|| provenance.cloned());

    let function = Func {
        id: dest_fn,
        name: metadata.functions[&dest_fn].clone(),
    };

    let node = Node {
        function,
        block: basic_block_idx.into(),
        statement_idx,
        kind: node_kind.clone(),
        source: source
            .as_ref()
            .and_then(|p| parent(&node_kind, p))
            .map(|pi| pi.nid),
        dest: event_metadata.destination.clone(),
        debug_info: event_metadata.debug_info.clone(),
        info: None,
    };

    let ptr_is_null = ptr.map_or(false, |ptr| ptr == 0);
    let graph_id = source
        .as_ref()
        .and_then(|p| parent(&node_kind, p))
        .map(|pi| pi.gid)
        .unwrap_or_else(|| graphs.graphs.push(Graph::new(ptr_is_null)));
    let node_id = graphs.graphs[graph_id].nodes.push(node);

    // Assert that we're not mixing null and non-null pointers
    assert!(
        graphs.graphs[graph_id].is_null == ptr_is_null,
        "graph[{}].is_null == {:?} != {:x?} for {:?}:{:?}",
        graph_id,
        graphs.graphs[graph_id].is_null,
        ptr,
        event,
        event_metadata
    );

    update_provenance(
        provenances,
        &event.kind,
        ProvenanceInfo::new(graph_id, node_id),
    );

    Some(node_id)
}

pub fn construct_pdg(events: &[Event], metadata: &Metadata) -> Graphs {
    let mut graphs = Graphs::new();
    let mut provenances = BTreeMap::new();
    let mut address_taken = AddressTaken::new();
    for event in events {
        add_node(
            &mut graphs,
            &mut provenances,
            &mut address_taken,
            event,
            metadata,
        );
    }
    // TODO(kkysen) check if I have to remove any `GraphId`s from `graphs.latest_assignment`
    graphs.graphs = graphs.graphs.into_iter().unique().collect();
    graphs
}
