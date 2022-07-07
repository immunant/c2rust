use enum_dispatch::enum_dispatch;
use once_cell::sync::{Lazy, OnceCell};
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::BufWriter;
use std::path::Path;
use std::sync::mpsc::{self, Receiver, SyncSender};
use std::sync::{Condvar, Mutex};
use std::thread;

use bincode;

use crate::events::{Event, EventKind};
use crate::metadata::{IWithMetadata, Metadata};

type AnyError = Box<dyn Error + Send + Sync + 'static>;

static FINISHED: Lazy<(Mutex<bool>, Condvar)> = Lazy::new(|| (Mutex::new(false), Condvar::new()));

pub struct Runtime {
    tx: SyncSender<Event>,
    finalized: OnceCell<()>,
}

impl Runtime {
    /// Initialize the [`Runtime`], which includes [`thread::spawn`]ing, so it must be run post-`main`.
    ///
    /// It returns an error if [`Backend::detect`] returns an error.
    pub fn try_init() -> Result<Self, AnyError> {
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

#[enum_dispatch]
trait IBackend {
    fn write(&mut self, event: Event);
}

pub struct DebugBackend {
    metadata: Metadata,
}

impl IBackend for DebugBackend {
    fn write(&mut self, event: Event) {
        eprintln!("{:?}", event.with_metadata(&self.metadata));
    }
}

pub struct LogBackend {
    writer: BufWriter<File>,
}

impl IBackend for LogBackend {
    fn write(&mut self, event: Event) {
        bincode::serialize_into(&mut self.writer, &event).unwrap();
    }
}

#[enum_dispatch(IBackend)]
pub enum Backend {
    Debug(DebugBackend),
    Log(LogBackend),
}

impl Backend {
    fn write_all(&mut self, rx: Receiver<Event>) {
        for event in rx {
            if matches!(event.kind, EventKind::Done) {
                return;
            }
            self.write(event);
        }
    }

    pub fn run(&mut self, rx: Receiver<Event>) {
        let (lock, cvar) = &*FINISHED;
        let mut finished = lock.lock().unwrap();
        self.write_all(rx);
        *finished = true;
        cvar.notify_one();
    }
}

impl DebugBackend {
    pub fn detect() -> Result<Self, AnyError> {
        let path = env::var_os("METADATA_FILE")
            .ok_or("Instrumentation requires the METADATA_FILE environment variable be set")?;
        let path = Path::new(&path);
        let file = File::open(path)?;
        let metadata: Metadata = bincode::deserialize_from(file)?;
        Ok(Self { metadata })
    }
}

impl LogBackend {
    pub fn detect() -> Result<Self, AnyError> {
        let path = env::var_os("INSTRUMENT_OUTPUT")
            .ok_or("Instrumentation requires the INSTRUMENT_OUTPUT environment variable be set")?;
        let file = File::create(&path)?;
        let writer = BufWriter::new(file);
        Ok(Self { writer })
    }
}

impl Backend {
    pub fn detect() -> Result<Self, AnyError> {
        let this = match env::var("INSTRUMENT_BACKEND").unwrap_or_default().as_str() {
            "log" => Self::Log(LogBackend::detect()?),
            "debug" => Self::Debug(DebugBackend::detect()?),
            _ => Self::Debug(DebugBackend::detect()?),
        };
        Ok(this)
    }
}

pub struct GlobalRuntime {
    runtime: OnceCell<Runtime>,
}

impl GlobalRuntime {
    /// Create a new [`GlobalRuntime`].
    ///
    /// This is not `pub` because it should only be called once below to create [`RUNTIME`],
    /// as it depends on [`FINISHED`], which is also a global.
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

    /// Try to initialize the [`GlobalRuntime`] with [`Runtime::try_init`].
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
