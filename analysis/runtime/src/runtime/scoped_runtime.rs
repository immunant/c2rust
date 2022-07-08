use std::{
    sync::mpsc::{self, SyncSender},
    thread,
};

use once_cell::sync::OnceCell;

use crate::events::Event;

use super::{
    backend::Backend,
    skip::{skip_event, SkipReason},
    AnyError, FINISHED,
};

pub struct Runtime {
    tx: SyncSender<Event>,
    finalized: OnceCell<()>,
}

impl Runtime {
    /// Initialize the [`Runtime`], which includes [`thread::spawn`]ing, so it must be run post-`main`.
    ///
    /// It returns an error if [`Backend::detect`] returns an error.
    ///
    /// It's only `pub(super)` as `super` is the scope of the global [`super::FINISHED`],
    /// which we have to prevent from being used multiple times.
    pub(super) fn try_init() -> Result<Self, AnyError> {
        let mut backend = Backend::detect()?;
        let (tx, rx) = mpsc::sync_channel(1024);
        thread::spawn(move || backend.run(rx));
        Ok(Self {
            tx,
            finalized: OnceCell::new(),
        })
    }

    /// Finalize the [`Runtime`], shutting it down.
    ///
    /// This can be called any number of times; it only finalizes once.
    ///
    /// This does the same thing as [`Runtime::drop`]
    /// except, of course, it's not a destructor.
    pub fn finalize(&self) {
        // only run finalizer once
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

    /// Send an [`Event`] to the [`Runtime`].
    ///
    /// If the [`Runtime`] has already been [`Runtime::finalize`]d,
    /// then the [`Event`] is silently dropped.
    /// Otherwise, it sends the [`Event`] to the channel,
    /// panicking if there is a [`SendError`](std::sync::mpsc::SendError).
    pub fn send_event(&self, event: Event) {
        match self.finalized.get() {
            None => {
                // `.unwrap()` as we're in no place to handle an error here,
                // unless we should silently drop the [`Event`] instead.
                self.tx.send(event).unwrap();
            }
            Some(()) => {
                // Silently drop the [`Event`] as the [`Runtime`] has already been [`Runtime::finalize`]d.
                skip_event(event, SkipReason::AfterMain);
            }
        }
    }
}

impl Drop for Runtime {
    /// Finalize the [`Runtime`], shutting it down.
    ///
    /// This does the same thing as [`Runtime::finalize`].
    fn drop(&mut self) {
        self.finalize();
    }
}
