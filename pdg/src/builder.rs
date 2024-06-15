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
        Alloc(..) | AddrOfLocal(..) | AddrOfConst(..) => None,
        _ => Some(obj),
    }
}

type AddressTaken = IndexSet<(FuncId, Local)>;

pub trait EventKindExt {
    fn ptr(&self, metadata: &EventMetadata) -> Option<Pointer>;
    fn to_node_kind(&self, func: FuncId, address_taken: &mut AddressTaken) -> Option<NodeKind>;
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
            CopyRef => return None, // FIXME
            ToInt(ptr) => ptr,
            Realloc { old_ptr, .. } => old_ptr,
            FromInt(lhs) => lhs,
            Alloc { ptr, .. } => ptr,
            AddrOfLocal(lhs, _, _) => lhs,
            AddrOfConst(ptr, _) => ptr,
            // Corner case: Offset(..) events with a base pointer of zero are special
            // because the result might be an actual pointer, e.g., c2rust will
            // emit a pointer increment `a += b` as `a = a.offset(b)` which we need
            // to ignore here if `a == 0` which is equivalent to `a = b`.
            Offset(0, _, ptr) => ptr,
            Offset(ptr, _, _) => ptr,
            Done | BeginFuncBody => return None,
        })
    }

    fn to_node_kind(&self, func: FuncId, address_taken: &mut AddressTaken) -> Option<NodeKind> {
        use EventKind::*;
        Some(match *self {
            Alloc { .. } => NodeKind::Alloc(1),
            Realloc { .. } => NodeKind::Alloc(1),
            Free { .. } => NodeKind::Free,
            CopyPtr(..) | CopyRef => NodeKind::Copy,
            Project(base_ptr, new_ptr) => NodeKind::Project(new_ptr - base_ptr),
            LoadAddr(..) => NodeKind::LoadAddr,
            StoreAddr(..) => NodeKind::StoreAddr,
            StoreAddrTaken(..) => NodeKind::StoreAddr,
            LoadValue(..) => NodeKind::LoadValue,
            StoreValue(..) => NodeKind::StoreValue,
            AddrOfLocal(_, local, _) => {
                // All but the first instance of AddrOfLocal in a given
                // function body are considered copies of that local's address
                let (_, inserted) = address_taken.insert_full((func, local));
                if inserted {
                    NodeKind::AddrOfLocal(local.as_u32().into())
                } else {
                    NodeKind::Copy
                }
            }
            AddrOfConst(_, size) => NodeKind::AddrOfConst(size),
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

    /// Initial node that created this object.
    nid: NodeId,
}

impl ProvenanceInfo {
    fn new(gid: GraphId, nid: NodeId) -> Self {
        Self { size: None, gid, nid }
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
        CopyPtr(ptr) |
        Project(_, ptr) |
        Offset(_, _, ptr) => {
            // Check that the pointer falls inside an existing allocation
            let need_insert = provenances.range(0..=ptr)
                .next_back()
                .map(|(start, pi)| {
                    // If the existing pointer has a size, check it
                    // Otherwise, insert ours if it's different
                    let size = match pi.size {
                        Some(size) => size,
                        None => return ptr != *start
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

                    log::warn!("0x{:x} outside of object bounds 0x{:x}-0x{:x}",
                        ptr, start, end);

                    false
                }).unwrap_or(true);

            if need_insert {
                provenances.insert(ptr, mapping);
            }
        }
        CopyRef => unimplemented!(),
        AddrOfLocal(ptr, _, size) => {
            // TODO: is this a local from another function?
            mapping.size = size.try_into().ok();
            provenances.insert(ptr, mapping);
        }
        AddrOfConst(ptr, size) => {
            mapping.size = size.try_into().ok();
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

    let node_kind = event.kind.to_node_kind(func.id, address_taken)?;
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
        provenances.range(0..=ptr)
            .next_back()
            .and_then(|(start, pi)| {
                let size = match pi.size {
                    Some(size) => size,
                    None if ptr == *start => return Some(pi),
                    None => return None
                };

                // If the current pointer falls outside the
                // original object, make a new graph for it
                let end = start.saturating_add(size);

                // This is intentionall greater-than because
                // one past the end is still a valid pointer
                if ptr > end {
                    None
                } else {
                    Some(pi)
                }
            })
    });
    let function = Func {
        id: dest_fn,
        name: metadata.functions[&dest_fn].clone(),
    };

    let node = Node {
        function,
        block: basic_block_idx.into(),
        statement_idx,
        kind: node_kind,
        source: provenance
            .as_ref()
            .and_then(|p| parent(&node_kind, p))
            .map(|pi| pi.nid),
        dest: event_metadata.destination.clone(),
        debug_info: event_metadata.debug_info.clone(),
        info: None,
    };

    let ptr_is_null = ptr.map_or(false, |ptr| ptr == 0);
    let graph_id = provenance
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
