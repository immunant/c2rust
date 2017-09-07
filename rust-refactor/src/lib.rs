#![feature(
    i128_type,
    rustc_private,
    trace_macros,
)]
extern crate arena;
extern crate ena;
extern crate libc;
extern crate diff;
#[macro_use] extern crate json;
#[macro_use] extern crate log;
extern crate regex;
extern crate rustc;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_metadata;
extern crate rustc_resolve;
extern crate rustc_trans;
extern crate syntax;
extern crate syntax_ext;
extern crate syntax_pos;

#[macro_use] mod macros;

pub mod ast_manip;

pub mod util;

pub mod rewrite;

pub mod analysis;

pub mod remove_paren;
pub mod span_fix;
pub mod pick_node;

pub mod path_edit;
pub mod api;
pub mod contains_mark;
pub mod reflect;
pub mod type_map;

pub mod matcher;

pub mod driver;

pub mod command;
pub mod interact;
pub mod plugin;

pub mod transform;
pub mod mark_adjust;
pub mod select;
pub mod print_spans;
