#[macro_use]
extern crate serde_derive;
extern crate bincode;
#[macro_use]
extern crate lazy_static;

pub mod span;
pub mod backend;
pub mod events;
mod handlers;

/// List of functions we want hooked for the lifetime analyis runtime.
pub const HOOK_FUNCTIONS: &[&'static str] = &[
    "malloc",
    "free",
    "calloc",
    "realloc",
    "reallocarray",
];

pub use self::span::{SourceSpan, BytePos};

pub use self::handlers::*;

pub struct Context { }

impl Drop for Context {
    fn drop(&mut self) {
        backend::finalize();
    }
}

pub fn init(span_filename: &str) {
    span::set_file(span_filename);
    backend::init();
}

pub fn context() -> Context {
    Context { }
}
