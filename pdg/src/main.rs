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

mod builder;
mod graph;
mod assert;
mod util;
mod query;

use builder::{construct_pdg, read_event_log};
use c2rust_analysis_rt::Runtime;
use color_eyre::eyre;
use std::{env, path::Path};

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    env_logger::init();
    let _runtime = Runtime::new();

    let event_trace_path = env::args().nth(1)
        .expect("Expected event trace file path as the first argument");
    let events = read_event_log(Path::new(event_trace_path.as_str()))?;

    // for event in &events {
    //     let mir_loc = mir_loc::get(event.mir_loc).unwrap();
    //     let kind = &event.kind;
    //     println!("{mir_loc:?} -> {kind:?}");
    // }

    let pdg = construct_pdg(&events);
    pdg.assert_all_tests();

    for (graph_id, graph) in pdg.graphs.iter_enumerated() {
        let needs_write = graph.needs_write_permission().map(|node_id| node_id.as_usize()).collect::<Vec<_>>();
        println!("{graph_id} {graph}");
        println!("nodes_that_need_write = {needs_write:?}");
        println!("\n");
    }

    Ok(())
}
