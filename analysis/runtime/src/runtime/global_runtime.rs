use std::process;

use once_cell::sync::OnceCell;

use crate::events::Event;

use super::{
    scoped_runtime::{ExistingRuntime, ScopedRuntime},
    skip::{skip_event, SkipReason},
    AnyError, Detect,
};

pub struct GlobalRuntime {
    runtime: OnceCell<ScopedRuntime>,
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
    /// If the [`ScopedRuntime`] has been initialized, then it sends the [`Event`] to it,
    /// panicking if there is a [`SendError`](std::sync::mpsc::SendError).
    ///
    /// If the [`ScopedRuntime`] has not yet been initialized,
    /// which is done so by [`crate::initialize`],
    /// which is called at the start of `main`,
    /// then this means we are executing pre-`main`,
    /// perhaps in an `.init_array` or `.ctors` section,
    /// and we cannot use threads yet as the Rust runtime is not fully initialized yet.
    /// Therefore, we silently drop the [`Event`].
    ///
    /// It also silently drops the [`Event`] if the [`ScopedRuntime`]
    /// has been [`ScopedRuntime::finalize`]d/[`GlobalRuntime::finalize`]d.
    pub fn send_event(&self, event: Event) {
        match self.runtime.get() {
            None => {
                // Silently drop the [`Event`] as the [`ScopedRuntime`] isn't ready/initialized yet.
                skip_event(event, SkipReason::BeforeMain);
            }
            Some(runtime) => {
                runtime.send_event(event);
            }
        }
    }

    /// Try to initialize the [`GlobalRuntime`] with [`ScopedRuntime::detect`].
    ///
    /// This (or [`GlobalRuntime::init`]), on [`RUNTIME`], should be called at the top of `main`.
    pub fn try_init(&self) -> Result<&ScopedRuntime, AnyError> {
        self.runtime.get_or_try_init(ScopedRuntime::detect)
    }

    /// Same as [`GlobalRuntime::try_init`], if there is an error,
    /// it is [`eprintln!`]ed [`std::process::exit`] called.
    ///
    /// This (or [`GlobalRuntime::try_init`]), on [`RUNTIME`], should be called at the top of `main`.
    pub fn init(&self) {
        if let Err(e) = self.try_init() {
            eprintln!("{e}");
            process::exit(1);
        }
    }

    /// Finalize the [`GlobalRuntime`] by finalizing the [`ScopedRuntime`] if it has been initialized.
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
