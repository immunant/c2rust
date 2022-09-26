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

pub mod assert;
pub mod builder;
pub mod graph;
pub mod info;
pub mod query;
pub mod util;
