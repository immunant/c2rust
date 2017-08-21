//! A generic wrapper for building parts of `idiomize` as a plugin.

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

#[macro_use] extern crate idiomize;
pub use idiomize::*;


fn mk<T: transform::Transform + 'static>(t: T) -> Box<command::Command> {
    Box::new(transform::TransformCommand(t))
}

// Adjust these lines to control what part of `idiomize` gets built.
#[path="src/analysis/mod.rs"]
mod plugin;
use self::plugin as analysis;

#[no_mangle]
pub fn register_commands(reg: &mut command::Registry) {
    plugin::register_commands(reg);
}
