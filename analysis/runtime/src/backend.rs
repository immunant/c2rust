use enum_dispatch::enum_dispatch;
use once_cell::race::OnceBool;
use once_cell::sync::{Lazy, OnceCell};
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::BufWriter;
use std::mem::forget;
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
}

impl Runtime {
    /// Initialize the [`Runtime`], which includes [`thread::spawn`]ing, so it must be run post-`main`.
    /// 
    /// It returns an error if [`Backend::detect`] returns an error.
    pub fn try_init() -> Result<Self, AnyError> {
        let mut backend = Backend::detect()?;
        let (tx, rx) = mpsc::sync_channel(1024);
        thread::spawn(move || backend.run(rx));
        Ok(Self { tx })
    }

    /// Finalize the [`Runtime`], shutting it down.
    ///
    /// This should not be called directly;
    /// call [`Runtime::finalize`] or [`Runtime::drop`] instead.
    ///
    /// This is only here because [`Drop::drop`] takes `&mut self` instead of `self`,
    /// but we want [`Runtime::finalize`] to take `self`.
    fn _finalize(&mut self) {
        // Notify the backend that we're done
        self.tx.send(Event::done()).unwrap();

        // Wait for the backend thread to finish
        let (lock, cvar) = &*FINISHED;
        let mut finished = lock.lock().unwrap();
        while !*finished {
            finished = cvar.wait(finished).unwrap();
        }
    }

    /// Finalize the [`Runtime`], shutting it down.
    ///
    /// This does the same thing as [`Runtime::drop`],
    /// except it consumes `self`.
    pub fn finalize(mut self) {
        self._finalize();
        forget(self); // don't `finalize` twice
    }
}

impl Drop for Runtime {
    /// Finalize the [`Runtime`], shutting it down.
    ///
    /// This does the same thing as [`Runtime::finalize`].
    fn drop(&mut self) {
        self._finalize();
    }
}

#[enum_dispatch]
trait IBackend {
    fn write(&mut self, event: Event);
}

struct DebugBackend {
    metadata: Metadata,
}

impl IBackend for DebugBackend {
    fn write(&mut self, event: Event) {
        eprintln!("{:?}", event.with_metadata(&self.metadata));
    }
}

struct LogBackend {
    writer: BufWriter<File>,
}

impl IBackend for LogBackend {
    fn write(&mut self, event: Event) {
        bincode::serialize_into(&mut self.writer, &event).unwrap();
    }
}

#[enum_dispatch(IBackend)]
enum Backend {
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
    /// panicking if there is a [`SendError`].
    /// 
    /// If the [`Runtime`] has not yet been initialized,
    /// which is done so by [`c2rust_analysis_rt::initialize`],
    /// which is called at the start of `main`,
    /// then this means we are executing pre-`main`,
    /// perhaps in an `.init_array` or `.ctors` section,
    /// and we cannot use threads yet as the Rust runtime is not fully initialized yet.
    /// Therefore, we silently drop the [`Event`].
    pub fn send_event(&self, event: Event) {
        match self.runtime.get() {
            None => {
                // silently drop the [`Event`] as the [`Runtime`] isn't ready yet
            },
            Some(runtime) => {
                runtime.tx.send(event).unwrap();
            },
        }
    }

    pub fn try_init(&self) -> Result<&Runtime, AnyError> {
        self.runtime.get_or_try_init(Runtime::try_init)
    }

    pub fn init(&self) {
        self.try_init().unwrap();
    }

    pub fn finalize(&mut self) {
        if let Some(runtime) = self.runtime.take() {
            runtime.finalize()
        }
    }
}

pub static RUNTIME: GlobalRuntime = GlobalRuntime::new();

// pub static TX: Lazy<SyncSender<Event>> = Lazy::new(|| {
//     let (tx, rx) = mpsc::sync_channel(1024);
//     thread::spawn(|| backend_thread(rx));
//     tx
// });

// pub fn init() {}

// pub fn finalize() {
//     // Notify the backend that we're done
//     TX.send(Event::done()).unwrap();

//     // Wait for the backend thread to finish
//     let (lock, cvar) = &*FINISHED;
//     let mut finished = lock.lock().unwrap();
//     while !*finished {
//         finished = cvar.wait(finished).unwrap();
//     }
// }

// fn backend_thread(rx: Receiver<Event>) {
//     let (lock, cvar) = &*FINISHED;
//     let mut finished = lock.lock().unwrap();

//     (match env::var("INSTRUMENT_BACKEND").unwrap_or_default().as_str() {
//         "log" => log,
//         "debug" => debug,
//         _ => debug,
//     })(rx);

//     *finished = true;
//     cvar.notify_one();
// }

// fn log(rx: Receiver<Event>) {
//     let path = env::var("INSTRUMENT_OUTPUT")
//         .expect("Instrumentation requires the INSTRUMENT_OUTPUT environment variable be set");
//     let mut out = BufWriter::new(
//         File::create(&path).unwrap_or_else(|_| panic!("Could not open output file: {:?}", path)),
//     );

//     for event in rx {
//         if let EventKind::Done = event.kind {
//             return;
//         }
//         bincode::serialize_into(&mut out, &event).unwrap();
//     }
// }

// fn debug(rx: Receiver<Event>) {
//     let metadata: Metadata = {
//         let path = env::var_os("METADATA_FILE")
//             .expect("Instrumentation requires the METADATA_FILE environment variable be set");
//         let path = Path::new(&path);
//         let file = File::open(path)
//             .unwrap_or_else(|_| panic!("Could not open span file: {:?}", path.display()));
//         bincode::deserialize_from(file).expect("Error deserializing span file")
//     };
//     for event in rx {
//         eprintln!("{:?}", event.with_metadata(&metadata));
//         if let EventKind::Done = event.kind {
//             return;
//         }
//     }
// }
