use once_cell::sync::OnceCell;

use crate::events::Event;

use super::{runtime::Runtime, AnyError};

pub struct GlobalRuntime {
    runtime: OnceCell<Runtime>,
}

impl GlobalRuntime {
    /// Create a new [`GlobalRuntime`].
    ///
    /// This is not `pub` because it should only be called once below to create [`RUNTIME`],
    /// as it depends on [`super::FINISHED`], which is also a global.
    const fn new() -> Self {
        Self {
            runtime: OnceCell::new(),
        }
    }

    /// Send an [`Event`] to the [`GlobalRuntime`].
    ///
    /// If the [`Runtime`] has been initialized, then it sends the [`Event`] to it,
    /// panicking if there is a [`SendError`](std::sync::mpsc::SendError).
    ///
    /// If the [`Runtime`] has not yet been initialized,
    /// which is done so by [`crate::initialize`],
    /// which is called at the start of `main`,
    /// then this means we are executing pre-`main`,
    /// perhaps in an `.init_array` or `.ctors` section,
    /// and we cannot use threads yet as the Rust runtime is not fully initialized yet.
    /// Therefore, we silently drop the [`Event`].
    /// 
    /// It also silently drops the [`Event`] if the [`Runtime`]
    /// has been [`Runtime::finalize`]d/[`GlobalRuntime::finalize`]d.
    pub fn send_event(&self, event: Event) {
        match self.runtime.get() {
            None => {
                // Silently drop the [`Event`] as the [`Runtime`] isn't ready/initialized yet.
            }
            Some(runtime) => {
                runtime.send_event(event);
            }
        }
    }

    /// Try to initialize the [`GlobalRuntime`] with `Runtime::try_init`.
    /// 
    /// This (or [`GlobalRuntime::init`]), on [`RUNTIME`], should be called at the top of `main`.
    pub fn try_init(&self) -> Result<&Runtime, AnyError> {
        self.runtime.get_or_try_init(Runtime::try_init)
    }

    /// Same as [`GlobalRuntime::try_init`] except the [`Result`] is `.unwrap()`ed.
    /// 
    /// This (or [`GlobalRuntime::try_init`]), on [`RUNTIME`], should be called at the top of `main`.
    pub fn init(&self) {
        self.try_init().unwrap();
    }

    /// Finalize the [`GlobalRuntime`] by finalizing the [`Runtime`] if it has been initialized.
    ///
    /// This can be called as many times as you want.
    ///
    /// When used from [`RUNTIME`], a `static`, this must be called at the end of `main` to properly finalize,
    /// as `static` destructors are not run.
    pub fn finalize(&self) {
        if let Some(runtime) = self.runtime.get() {
            runtime.finalize();
        }
    }
}

pub static RUNTIME: GlobalRuntime = GlobalRuntime::new();
