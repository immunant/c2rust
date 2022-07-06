pub mod backend;
pub mod events;
mod handlers;
pub mod mir_loc;

/// List of functions we want hooked for the lifetime analyis runtime.
pub const HOOK_FUNCTIONS: &[&str] = &[
    "malloc",
    "free",
    "calloc",
    "realloc",
    "reallocarray",
    "offset",
];

pub use self::mir_loc::{DefPathHash, Metadata, MirLoc, MirLocId, MirPlace, MirProjection};

pub use self::handlers::*;

pub fn initialize() {
    backend::init();
}

pub fn finalize() {
    backend::finalize();
}

pub struct Runtime;

impl Runtime {
    /// An `impl `[`Default`] doesn't make sense when it's a RAII type that initializes a runtime.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        initialize();
        Self
    }
}

impl Drop for Runtime {
    fn drop(&mut self) {
        finalize();
    }
}
