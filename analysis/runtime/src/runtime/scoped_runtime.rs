use std::{
    sync::mpsc::{self, SyncSender},
    thread,
};

use once_cell::sync::OnceCell;

use crate::events::Event;

use super::{
    backend::Backend,
    skip::{skip_event, SkipReason},
    AnyError, Detect, FINISHED,
};

pub(super) trait ExistingRuntime {
    /// Finalize the [`ExistingRuntime`].
    ///
    /// Similar to [`Drop::drop`], except it takes `&self`, not `&mut self`,
    /// so it can be run in a [`OnceCell`].
    fn finalize(&self);

    fn send_event(&self, event: Event);
}

trait Runtime: ExistingRuntime + Sized {
    fn try_init(backend: Backend) -> Result<Self, AnyError>;
}

pub struct ScopedRuntime {
    tx: SyncSender<Event>,
    finalized: OnceCell<()>,
}

impl ExistingRuntime for ScopedRuntime {
    /// Finalize the [`ScopedRuntime`], shutting it down.
    ///
    /// This can be called any number of times; it only finalizes once.
    ///
    /// This does the same thing as [`ScopedRuntime::drop`]
    /// except, of course, it's not a destructor.
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

    /// Send an [`Event`] to the [`ScopedRuntime`].
    ///
    /// If the [`ScopedRuntime`] has already been [`ScopedRuntime::finalize`]d,
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
                // Silently drop the [`Event`] as the [`ScopedRuntime`] has already been [`ScopedRuntime::finalize`]d.
                skip_event(event, SkipReason::AfterMain);
            }
        }
    }
}

impl Drop for ScopedRuntime {
    /// Finalize the [`ScopedRuntime`], shutting it down.
    ///
    /// This does the same thing as [`ScopedRuntime::finalize`].
    fn drop(&mut self) {
        self.finalize();
    }
}

impl Runtime for ScopedRuntime {
    /// Initialize the [`ScopedRuntime`], which includes [`thread::spawn`]ing,
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

impl Detect for ScopedRuntime {
    fn detect() -> Result<Self, AnyError> {
        let backend = Backend::detect()?;
        Self::try_init(backend)
    }
}
