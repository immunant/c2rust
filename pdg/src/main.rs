#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_driver;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_mir_transform;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

mod graph;
mod builder;

use builder::read_event_log;
use std::env;
use lazy_static::lazy_static;

lazy_static! {
    static ref EVENT_TRACE_FILE_PATH: String = {
        env::args()
            .skip(1)
            .next()
            .expect("Expected event trace file path as the first argument")
    };
}

fn main() {
    let events = read_event_log(EVENT_TRACE_FILE_PATH.to_string());
    for event in events {
        println!("{:?}", event.kind);
    }
}
