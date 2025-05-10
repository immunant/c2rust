use std::{
    sync::{Arc, Mutex},
    thread,
};

use crossbeam_queue::ArrayQueue;
use crossbeam_utils::Backoff;
use enum_dispatch::enum_dispatch;
use once_cell::sync::OnceCell;

use crate::{
    events::Event,
    parse::{self, AsStr, GetChoices},
};

use super::{
    backend::{Backend, WriteEvent},
    skip::{skip_event, SkipReason},
    AnyError, Detect, FINISHED,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RuntimeKind {
    MainThread,
    BackgroundThread,
}

impl AsStr for RuntimeKind {
    fn as_str(&self) -> &'static str {
        match self {
            Self::MainThread => "fg",
            Self::BackgroundThread => "bg",
        }
    }
}

impl GetChoices for RuntimeKind {
    fn choices() -> &'static [Self] {
        &[Self::MainThread, Self::BackgroundThread]
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
    MainThread(MainThreadRuntime),
    BackgroundThread(BackgroundThreadRuntime),
}

impl ScopedRuntime {
    pub fn detect_kind(kind: RuntimeKind) -> Result<Self, AnyError> {
        let backend = Backend::detect()?;
        let this = match kind {
            RuntimeKind::MainThread => Self::MainThread(MainThreadRuntime::try_init(backend)?),
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

pub struct MainThreadRuntime {
    backend: Mutex<Backend>,
}

impl ExistingRuntime for MainThreadRuntime {
    fn finalize(&self) {
        self.backend.lock().unwrap().flush();
    }

    // # Async-signal-safety: NOT SAFE!!!
    // Do not use this with programs that install signal handlers.
    fn send_event(&self, event: Event) {
        self.backend.lock().unwrap().write(event);
    }
}

impl Runtime for MainThreadRuntime {
    fn try_init(backend: Backend) -> Result<Self, AnyError> {
        let backend = Mutex::new(backend);
        Ok(Self { backend })
    }
}

pub struct BackgroundThreadRuntime {
    tx: Arc<ArrayQueue<Event>>,
    finalized: OnceCell<()>,
}

impl BackgroundThreadRuntime {
    fn push_event(&self, mut event: Event, can_sleep: bool) {
        // # Async-signal-safety: This needs `can_sleep == false` if called from
        // a signal handler; in that case, it spins instead of sleeping
        // which should be safe. `ArrayQueue::push` is backed by a fixed-size
        // array so it does not allocate.
        let backoff = Backoff::new();
        while let Err(event_back) = self.tx.push(event) {
            if can_sleep {
                backoff.snooze();
            } else {
                // We have no choice but to spin here because
                // we might be inside a signal handler
                backoff.spin();
            }
            event = event_back;
        }
    }
}

impl ExistingRuntime for BackgroundThreadRuntime {
    fn finalize(&self) {
        // Only run the finalizer once.
        self.finalized.get_or_init(|| {
            // Notify the backend that we're done.
            self.push_event(Event::done(), true);

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
                // # Async-signal-safety: `push_event` is safe if `can_sleep == false`
                self.push_event(event, false);
            }
            Some(()) => {
                // Silently drop the [`Event`] as the [`BackgroundThreadRuntime`]
                // has already been [`BackgroundThreadRuntime::finalize`]d.
                //
                // # Async-signal-safety: `skip_event(_, AfterMain)` is NOT SAFE;
                // however, see the comment in `skip_event` for an explanation
                // of why this will probably be okay in practice.
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
        let tx = Arc::new(ArrayQueue::new(1 << 20));
        let rx = Arc::clone(&tx);
        thread::spawn(move || backend.run(rx));
        Ok(Self {
            tx,
            finalized: OnceCell::new(),
        })
    }
}
