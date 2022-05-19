use crate::graph::{Graph, GraphId, Graphs, Node, NodeId, NodeKind};
use bincode;
use c2rust_analysis_rt::events::{Event, EventKind};
use c2rust_analysis_rt::mir_loc::Metadata;
use rustc_middle::mir::Field;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;

pub fn read_event_log(path: String) -> Vec<Event> {
    let file = File::open(path).unwrap();
    let mut events = vec![];
    let mut reader = BufReader::new(file);
    loop {
        match bincode::deserialize_from(&mut reader) {
            Ok(e) => events.push(e),
            _ => break,
        }
    }
    events
}

pub fn read_metadata(path: String) -> Metadata {
    let file = File::open(path).unwrap();
    bincode::deserialize_from(file).unwrap()
}

/** return the ptr referenced by an EventKind */
fn get_ptr(kind: &EventKind) -> Option<&usize> {
    Some(match kind {
        EventKind::Copy(ptr) => ptr,
        EventKind::Field(ptr, id) => ptr,
        EventKind::Alloc { size, ptr } => ptr,
        EventKind::Free { ptr } => ptr,
        EventKind::Realloc { old_ptr, size, new_ptr } => old_ptr,
        EventKind::Arg(ptr) => ptr,
        EventKind::Ret(ptr) => ptr,
        EventKind::Done => return None,
        EventKind::LoadAddr(ptr) => ptr,
        EventKind::StoreAddr(ptr) => ptr,
    })
}

/** return the new ptr created by an EventKind */
fn get_new_ptr(kind: &EventKind) -> Option<&usize> {
    Some(match kind {
        EventKind::Field(ptr, id) => todo!("ptr + id to offset"),
        EventKind::Alloc { size, ptr } => ptr,
        EventKind::Realloc { new_ptr, .. } => new_ptr,
        _ => return None,
    })
}

pub fn event_to_node_kind(event: &Event) -> Option<NodeKind> {
    match event.kind {
        EventKind::Alloc { .. } => Some(NodeKind::Malloc(1)),
        EventKind::Realloc { .. } => Some(NodeKind::Malloc(1)),
        EventKind::Free { .. } => Some(NodeKind::Free),
        EventKind::Copy(..) => Some(NodeKind::Copy),
        EventKind::Field(_, field) => Some(NodeKind::Field(Field::from(field))),
        EventKind::LoadAddr(..) => Some(NodeKind::LoadAddr),
        EventKind::StoreAddr(..) => Some(NodeKind::StoreAddr),
        _ => None
    }
}

pub fn add_node(graphs: &mut Graphs, origins: &mut HashMap<NodeId, GraphId>, provenances: &mut HashMap<usize, NodeId>, event: &Event) -> Option<NodeId> {
    let node_kind = match event_to_node_kind(event) {
        Some(kind) => kind,
        None => return None,
    };
    let (function, block, index) = todo!("event.mir_loc");
    let source = get_ptr(&event.kind).and_then(|p| provenances.get(p)).cloned();

    let node = Node {
        function,
        block,
        index,
        kind: node_kind,
        source,
        dest: todo!("event.dest"),
    };

    let node_id = {
        let graph_id = source.and_then(|source_node_id| {
            /* search for existing graph */
            origins.get(&source_node_id).cloned()
        });
        let graph_id = graph_id.unwrap_or_else(|| {
            /* if no existing object, construct new graph */
            graphs.graphs.push(Graph::new())
        });
        graphs.graphs[graph_id].nodes.push(node)
    };

    if let Some(&ptr) = get_new_ptr(&event.kind) {
        provenances.insert(ptr, node_id);
    }
    Some(node_id)
}

pub fn construct_pdg(events: &Vec<Event>) -> Graphs {
    let mut graphs = Graphs::new();

    let mut origins = HashMap::<NodeId, GraphId>::new();
    let mut provenances = HashMap::<usize, NodeId>::new();
    for event in events {
        add_node(&mut graphs, &mut origins, &mut provenances, event);
    }

    graphs
}
