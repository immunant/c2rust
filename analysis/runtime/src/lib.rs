pub mod runtime;
pub mod events;
mod handlers;
pub mod metadata;
pub mod mir_loc;


pub use handlers::*;
use runtime::global_runtime::RUNTIME;

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
    RUNTIME.init();
}

pub fn finalize() {
    RUNTIME.finalize();
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
