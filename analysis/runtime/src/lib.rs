pub mod events;
mod handlers;
pub mod metadata;
pub mod mir_loc;
pub mod runtime;

pub use handlers::*;
use runtime::{global_runtime::RUNTIME, skip::notify_if_events_were_skipped_before_main};

/// List of functions we want hooked for the lifetime analyis runtime.
pub const HOOK_FUNCTIONS: &[&str] = &[
    "malloc",
    "free",
    "calloc",
    "realloc",
    "reallocarray",
    "offset",
];

pub fn initialize() {
    notify_if_events_were_skipped_before_main();
    RUNTIME.init();
}

pub fn finalize() {
    RUNTIME.finalize();
}
