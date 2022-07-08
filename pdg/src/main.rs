#![feature(min_specialization)]
#![feature(rustc_private)]
#![feature(map_try_insert)]

extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_mir_transform;
extern crate rustc_serialize;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

mod assert;
mod builder;
mod graph;
mod query;
mod util;

use builder::{construct_pdg, read_event_log};
use color_eyre::eyre;
use std::{env, path::Path};

use crate::builder::read_metadata;

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    env_logger::init();

    let metadata_path = env::args_os()
        .nth(2)
        .expect("Expected metadata file path as the 1st argument");
    let event_trace_path = env::args_os()
        .nth(1)
        .expect("Expected event trace file path as the 2nd argument");

    let metadata = read_metadata(Path::new(&metadata_path))?;
    let events = read_event_log(Path::new(&event_trace_path))?;

    // for event in &events {

    //     let mir_loc = metadata.get(event.mir_loc);
    //     let mir_loc = mir_loc.with_metadata(&metadata);
    //     let kind = &event.kind;
    //     println!("{mir_loc:?} -> {kind:?}");
    // }

    let pdg = construct_pdg(&events, &metadata);
    pdg.assert_all_tests();

    for (graph_id, graph) in pdg.graphs.iter_enumerated() {
        let needs_write = graph
            .needs_write_permission()
            .map(|node_id| node_id.as_usize())
            .collect::<Vec<_>>();
        println!("{graph_id} {graph}");
        println!("nodes_that_need_write = {needs_write:?}");
        println!("\n");
    }

    let num_graphs = pdg.graphs.len();
    let num_nodes = pdg
        .graphs
        .iter()
        .map(|graph| graph.nodes.len())
        .sum::<usize>();
    dbg!(num_graphs);
    dbg!(num_nodes);

    Ok(())
}
