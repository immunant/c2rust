use crate::graph::{Graph, Graphs, Node, NodeKind};
use bincode;
use c2rust_analysis_rt::events::{Event, EventKind};
use c2rust_analysis_rt::mir_loc::Metadata;
use rustc_middle::mir::Field;
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

pub fn add_node(graphs: &mut Graphs, event: &Event) {
    let node_kind = event_to_node_kind(event);
    /* search for existing object */

    /* if no existing object, construct new object with event */

    /*
        if existing object, add to tree, point back to node that created the input
            - for copy, point to the thing being copied
            - for field, point to the base
            - for offset, point to the base
            - for free, point to
    */
    match event.kind {
        e @ (EventKind::Alloc { .. } | EventKind::Realloc { .. }) => (),
        _ => (),
    }
}

pub fn construct_pdg(events: &Vec<Event>) -> Graphs {
    let mut graphs = Graphs::new();

    for event in events {
        add_node(&mut graphs, event)
    }

    graphs
}
