use std::{
    sync::mpsc::{self, SyncSender},
    thread,
};

use enum_dispatch::enum_dispatch;
use once_cell::sync::OnceCell;

use crate::{
    events::Event,
    parse::{self, AsStr, GetChoices},
};

use super::{
    backend::Backend,
    skip::{skip_event, SkipReason},
    AnyError, Detect, FINISHED,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RuntimeKind {
    BackgroundThread,
}

impl AsStr for RuntimeKind {
    fn as_str(&self) -> &'static str {
        match self {
            Self::BackgroundThread => "bg",
        }
    }
}

impl GetChoices for RuntimeKind {
    fn choices() -> &'static [Self] {
        &[Self::BackgroundThread]
    }
}

impl Default for RuntimeKind {
    fn default() -> Self {
        Self::BackgroundThread
    }
}

impl Detect for RuntimeKind {
    fn detect() -> Result<Self, AnyError> {
        Ok(parse::env::one_of("INSTRUMENT_RUNTIME").cloned()?)
    }
}

#[enum_dispatch]
pub trait ExistingRuntime {
    /// Finalize the [`ExistingRuntime`].
    ///
    /// Must be idempotent, i.e. able to be called multiple times.
    ///
    /// Similar to [`Drop::drop`], except it takes `&self`, not `&mut self`,
    /// so it can be run in a [`OnceCell`].
    fn finalize(&self);

    fn send_event(&self, event: Event);
}

trait Runtime: ExistingRuntime + Sized {
    fn try_init(backend: Backend) -> Result<Self, AnyError>;
}

#[enum_dispatch(ExistingRuntime)]
pub enum ScopedRuntime {
    BackgroundThread(BackgroundThreadRuntime),
}

impl ScopedRuntime {
    pub fn detect_kind(kind: RuntimeKind) -> Result<Self, AnyError> {
        let backend = Backend::detect()?;
        let this = match kind {
            RuntimeKind::BackgroundThread => {
                Self::BackgroundThread(BackgroundThreadRuntime::try_init(backend)?)
            }
        };
        Ok(this)
    }
}

impl Detect for ScopedRuntime {
    fn detect() -> Result<Self, AnyError> {
        Self::detect_kind(RuntimeKind::detect()?)
    }
}

pub struct BackgroundThreadRuntime {
    tx: SyncSender<Event>,
    finalized: OnceCell<()>,
}

impl ExistingRuntime for BackgroundThreadRuntime {
    fn finalize(&self) {
        // Only run the finalizer once.
        self.finalized.get_or_init(|| {
            // Notify the backend that we're done.
            self.tx.send(Event::done()).unwrap();

            // Wait for the backend thread to finish.
            let (lock, cvar) = &*FINISHED;
            let mut finished = lock.lock().unwrap();
            while !*finished {
                finished = cvar.wait(finished).unwrap();
            }
        });
        // Don't need to `forget(self)` since the finalizer can only run once anyways.
    }

    /// Send an [`Event`] to the [`BackgroundThreadRuntime`].
    ///
    /// If the [`BackgroundThreadRuntime`] has already been [`BackgroundThreadRuntime::finalize`]d,
    /// then the [`Event`] is silently dropped.
    /// Otherwise, it sends the [`Event`] to the channel,
    /// panicking if there is a [`SendError`](std::sync::mpsc::SendError).
    fn send_event(&self, event: Event) {
        match self.finalized.get() {
            None => {
                // `.unwrap()` as we're in no place to handle an error here,
                // unless we should silently drop the [`Event`] instead.
                self.tx.send(event).unwrap();
            }
            Some(()) => {
                // Silently drop the [`Event`] as the [`BackgroundThreadRuntime`] has already been [`BackgroundThreadRuntime::finalize`]d.
                skip_event(event, SkipReason::AfterMain);
            }
        }
    }
}

impl Drop for BackgroundThreadRuntime {
    /// Finalize the [`BackgroundThreadRuntime`], shutting it down.
    ///
    /// This does the same thing as [`BackgroundThreadRuntime::finalize`].
    fn drop(&mut self) {
        self.finalize();
    }
}

impl Runtime for BackgroundThreadRuntime {
    /// Initialize the [`BackgroundThreadRuntime`], which includes [`thread::spawn`]ing,
    /// so it must be run post-`main`.
    fn try_init(mut backend: Backend) -> Result<Self, AnyError> {
        let (tx, rx) = mpsc::sync_channel(1024);
        thread::spawn(move || backend.run(rx));
        Ok(Self {
            tx,
            finalized: OnceCell::new(),
        })
    }
}
