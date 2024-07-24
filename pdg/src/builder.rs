use crate::graph::{Graph, GraphId, Graphs, Node, NodeId, NodeKind};
use c2rust_analysis_rt::events::{Event, EventKind, Pointer};
use c2rust_analysis_rt::metadata::Metadata;
use c2rust_analysis_rt::mir_loc::{EventMetadata, Func, FuncId, Local, MirLoc, TransferKind};
use color_eyre::eyre;
use fs_err::File;
use indexmap::IndexSet;
use itertools::Itertools;
use std::collections::HashMap;
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

fn parent(e: &NodeKind, obj: (GraphId, NodeId)) -> Option<(GraphId, NodeId)> {
    use NodeKind::*;
    match e {
        Alloc(..) | AddrOfLocal(..) => None,
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
            AddrOfLocal(lhs, _) => lhs,
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
            Project(base_ptr, new_ptr, idx) => NodeKind::Project(new_ptr - base_ptr, idx),
            LoadAddr(..) => NodeKind::LoadAddr,
            StoreAddr(..) => NodeKind::StoreAddr,
            StoreAddrTaken(..) => NodeKind::StoreAddr,
            LoadValue(..) => NodeKind::LoadValue,
            StoreValue(..) => NodeKind::StoreValue,
            AddrOfLocal(_, local) => {
                // All but the first instance of AddrOfLocal in a given
                // function body are considered copies of that local's address
                let (_, inserted) = address_taken.insert_full((func, local));
                if inserted {
                    NodeKind::AddrOfLocal(local.as_u32().into())
                } else {
                    NodeKind::Copy
                }
            }
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

fn update_provenance(
    provenances: &mut HashMap<Pointer, (GraphId, NodeId)>,
    event_kind: &EventKind,
    metadata: &EventMetadata,
    mapping: (GraphId, NodeId),
) {
    use EventKind::*;
    match *event_kind {
        Alloc { ptr, .. } => {
            provenances.insert(ptr, mapping);
        }
        CopyPtr(ptr) => {
            // only insert if not already there
            if let Err(..) = provenances.try_insert(ptr, mapping) {
                log::warn!("0x{:x} already has a source", ptr);
            }
        }
        Realloc { new_ptr, .. } => {
            provenances.insert(new_ptr, mapping);
        }
        Offset(_, _, new_ptr) => {
            provenances.insert(new_ptr, mapping);
        }
        CopyRef => {
            provenances.insert(metadata.destination.clone().unwrap().local.into(), mapping);
        }
        AddrOfLocal(ptr, _) => {
            provenances.insert(ptr, mapping);
        }
        _ => {}
    }
}

pub fn add_node(
    graphs: &mut Graphs,
    provenances: &mut HashMap<Pointer, (GraphId, NodeId)>,
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
    let (src_fn, dest_fn) = match event_metadata.transfer_kind {
        TransferKind::None => (this_id, this_id),
        TransferKind::Arg(id) => (this_id, id),
        TransferKind::Ret(id) => (id, this_id),
    };

    if let TransferKind::Arg(_) = event_metadata.transfer_kind {
        // FIXME: this is a special case for arguments
        basic_block_idx = 0;
        statement_idx = 0;
    }

    let provenance = event
        .kind
        .ptr(event_metadata)
        .and_then(|ptr| provenances.get(&ptr).cloned());
    let direct_source = provenance.and_then(|(gid, _last_nid_ref)| {
        graphs.graphs[gid]
            .nodes
            .iter()
            .rposition(|n| {
                if let (Some(d), Some(s)) = (&n.dest, &event_metadata.source) {
                    d == s
                } else {
                    false
                }
            })
            .map(|nid| (gid, NodeId::from(nid)))
    });

    let source = direct_source.or_else(|| {
        event_metadata.source.as_ref().and_then(|src| {
            let latest_assignment = graphs.latest_assignment.get(&(src_fn, src.local)).cloned();
            if !src.projection.is_empty() {
                if let Some((gid, _)) = latest_assignment {
                    if let Some((nid, n)) = graphs.graphs[gid].nodes.iter_enumerated().rev().next()
                    {
                        if let NodeKind::Project(..) = n.kind {
                            return Some((gid, nid));
                        }
                    }
                }
            }

            if !matches!(event.kind, EventKind::AddrOfLocal(..)) && src.projection.is_empty() {
                latest_assignment
            } else if let EventKind::Project(..) = event.kind {
                latest_assignment
            } else {
                provenance
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
        source: source
            .and_then(|p| parent(&node_kind, p))
            .map(|(_, nid)| nid),
        dest: event_metadata.destination.clone(),
        debug_info: event_metadata.debug_info.clone(),
        info: None,
    };

    let graph_id = source
        .or(direct_source)
        .or(provenance)
        .and_then(|p| parent(&node_kind, p))
        .map(|(gid, _)| gid)
        .unwrap_or_else(|| graphs.graphs.push(Graph::new()));
    let node_id = graphs.graphs[graph_id].nodes.push(node);

    update_provenance(
        provenances,
        &event.kind,
        event_metadata,
        (graph_id, node_id),
    );

    if let Some(dest) = &event_metadata.destination {
        let unique_place = (dest_fn, dest.local);
        let last_setting = (graph_id, node_id);

        if let Some(last @ (last_gid, last_nid)) =
            graphs.latest_assignment.insert(unique_place, last_setting)
        {
            if !dest.projection.is_empty()
                && graphs.graphs[last_gid].nodes[last_nid]
                    .dest
                    .as_ref()
                    .unwrap()
                    .projection
                    .is_empty()
            {
                graphs.latest_assignment.insert(unique_place, last);
            }
        }
    }

    Some(node_id)
}

pub fn construct_pdg(events: &[Event], metadata: &Metadata) -> Graphs {
    let mut graphs = Graphs::new();
    let mut provenances = HashMap::new();
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
