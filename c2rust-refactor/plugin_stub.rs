//! A generic wrapper for building parts of `c2rust-refactor` as a plugin.

#![feature(
    rustc_private,
    trace_macros,
)]

pub use c2rust_refactor::*;

fn mk<T: transform::Transform + 'static>(t: T) -> Box<command::Command> {
    Box::new(transform::TransformCommand(t))
}

// Adjust these lines to control what part of `c2rust-refactor` gets built.
//#[path="src/analysis/mod.rs"]
#[path="src/transform/retype.rs"]
mod plugin;
//use self::plugin as analysis;

#[no_mangle]
pub fn register_commands(reg: &mut command::Registry) {
    plugin::register_commands(reg);
}
