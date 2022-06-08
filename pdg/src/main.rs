#![feature(min_specialization)]
#![feature(rustc_private)]
#![feature(map_try_insert)]
extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_driver;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_mir_transform;
extern crate rustc_serialize;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_hir;
extern crate rustc_data_structures;

mod builder;
mod graph;


use builder::{read_event_log, construct_pdg};
use c2rust_analysis_rt::mir_loc;
use lazy_static::lazy_static;
use std::env;

lazy_static! {
    static ref EVENT_TRACE_FILE_PATH: String = {
        env::args()
            .skip(1)
            .next()
            .expect("Expected event trace file path as the first argument")
    };
}

fn main() {
    env_logger::init();
    c2rust_analysis_rt::initialize();

    let events = read_event_log(EVENT_TRACE_FILE_PATH.to_string());

    for event in &events {
        let mir_loc = mir_loc::get(event.mir_loc).unwrap();
        println!("{:?} -> {:?}", mir_loc, event.kind);
    }

    let pdg = construct_pdg(&events);
    for (g, graph) in pdg.graphs.iter().enumerate() {
        println!("-- Object {:?} ---", g);
        for (n, node) in graph.nodes.iter().enumerate() {
            println!("{:?}:{:?}", n, node)
        }
        println!()
    }

    c2rust_analysis_rt::finalize();
}
