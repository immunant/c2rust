#[macro_use]
extern crate serde_derive;
extern crate bincode;
#[macro_use]
extern crate lazy_static;

mod span;
mod debug;

/// List of functions we want hooked for the lifetime analyis runtime.
pub const HOOK_FUNCTIONS: &[&'static str] = &[
    "malloc",
    "free",
    "calloc",
    "realloc",
    "reallocarray",
];

pub use self::span::{SourceSpan, BytePos, set_span_file};

pub use self::debug::*;
