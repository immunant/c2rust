use crate::graph::{Graph, GraphId, Graphs, Node, NodeId, NodeKind};
use c2rust_analysis_rt::events::{Event, EventKind, Pointer};
use c2rust_analysis_rt::metadata::Metadata;
use c2rust_analysis_rt::mir_loc::{EventMetadata, Func, MirLoc, TransferKind};
use color_eyre::eyre;
use fs_err::File;
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
    let metadata = bincode::deserialize(&bytes)?;
    Ok(metadata)
}

pub trait EventKindExt {
    fn ptr(&self, metadata: &EventMetadata) -> Option<Pointer>;
    fn has_parent(&self) -> bool;
    fn parent(&self, obj: (GraphId, NodeId)) -> Option<(GraphId, NodeId)>;
    fn to_node_kind(&self) -> Option<NodeKind>;
}

impl EventKindExt for EventKind {
    /// return the ptr of interest for a particular event
    fn ptr(&self, _metadata: &EventMetadata) -> Option<Pointer> {
        use EventKind::*;
        Some(match *self {
            CopyPtr(lhs) => lhs,
            Field(ptr, ..) => ptr,
            Free { ptr } => ptr,
            Ret(ptr) => ptr,
            LoadAddr(ptr) => ptr,
            StoreAddr(ptr) => ptr,
            LoadValue(ptr) => ptr,
            StoreValue(ptr) => ptr,
            CopyRef => return None, // FIXME
            ToInt(ptr) => ptr,
            Realloc { old_ptr, .. } => old_ptr,
            FromInt(lhs) => lhs,
            Alloc { ptr, .. } => ptr,
            AddrOfLocal(lhs, _) => lhs,
            Offset(ptr, _, _) => ptr,
            Done => return None,
        })
    }

    fn has_parent(&self) -> bool {
        use EventKind::*;
        !matches!(
            self,
            Realloc { new_ptr: _, .. } | Alloc { ptr: _, .. } | AddrOfLocal(_, _) | Done
        )
    }

    fn parent(&self, obj: (GraphId, NodeId)) -> Option<(GraphId, NodeId)> {
        self.has_parent().then(|| obj)
    }

    fn to_node_kind(&self) -> Option<NodeKind> {
        use EventKind::*;
        Some(match *self {
            Alloc { .. } => NodeKind::Alloc(1),
            Realloc { .. } => NodeKind::Alloc(1),
            Free { .. } => NodeKind::Free,
            CopyPtr(..) | CopyRef => NodeKind::Copy,
            Field(_, field) => NodeKind::Field(field.into()),
            LoadAddr(..) => NodeKind::LoadAddr,
            StoreAddr(..) => NodeKind::StoreAddr,
            LoadValue(..) => NodeKind::LoadValue,
            StoreValue(..) => NodeKind::StoreValue,
            AddrOfLocal(_, local) => NodeKind::AddrOfLocal(local.as_u32().into()),
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
                log::warn!("0x{:x} doesn't have a source", ptr);
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
    event: &Event,
    metadata: &Metadata,
) -> Option<NodeId> {
    let node_kind = event.kind.to_node_kind()?;

    let MirLoc {
        func,
        mut basic_block_idx,
        mut statement_idx,
        metadata: event_metadata,
    } = metadata.get(event.mir_loc);

    let this_func_hash = func.def_path_hash;
    let (src_fn, dest_fn) = match event_metadata.transfer_kind {
        TransferKind::None => (this_func_hash, this_func_hash),
        TransferKind::Arg(p) => (this_func_hash, p),
        TransferKind::Ret(p) => (p, this_func_hash),
    };

    if let TransferKind::Arg(_) = event_metadata.transfer_kind {
        // FIXME: this is a special case for arguments
        basic_block_idx = 0;
        statement_idx = 0;
    }

    let head = event
        .kind
        .ptr(event_metadata)
        .and_then(|ptr| provenances.get(&ptr).cloned());
    let ptr = head.and_then(|(gid, _last_nid_ref)| {
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

    let source = ptr.or_else(|| {
        event_metadata.source.as_ref().and_then(|src| {
            let latest_assignment = graphs.latest_assignment.get(&(src_fn, src.local)).cloned();
            if !src.projection.is_empty() {
                if let Some((gid, _)) = latest_assignment {
                    if let Some((nid, n)) = graphs.graphs[gid].nodes.iter_enumerated().rev().next()
                    {
                        if let NodeKind::Field(..) = n.kind {
                            return Some((gid, nid));
                        }
                    }
                }
            }

            if src.projection.is_empty() {
                latest_assignment
            } else if let EventKind::Field(..) = event.kind {
                latest_assignment
            } else {
                head
            }
        })
    });

    let function = Func {
        def_path_hash: dest_fn,
        name: metadata.functions[&dest_fn].clone(),
    };

    let node = Node {
        function,
        block: basic_block_idx.into(),
        statement_idx,
        kind: node_kind,
        source: source
            .and_then(|p| event.kind.parent(p))
            .map(|(_, nid)| nid),
        dest: event_metadata.destination.clone(),
        node_info: None,
        debug_info: event_metadata.debug_info.clone(),
    };

    let graph_id = source
        .or(ptr)
        .or(head)
        .and_then(|p| event.kind.parent(p))
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
    for event in events {
        add_node(&mut graphs, &mut provenances, event, metadata);
    }
    // TODO(kkysen) check if I have to remove any `GraphId`s from `graphs.latest_assignment`
    graphs.graphs = graphs.graphs.into_iter().unique().collect();
    graphs
}
