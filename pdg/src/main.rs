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

fn main() {
    println!("Hello");
}
