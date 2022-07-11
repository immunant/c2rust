use enum_dispatch::enum_dispatch;
use fs_err::{File, OpenOptions};
use std::env;
use std::io::BufWriter;
use std::sync::mpsc::Receiver;

use bincode;

use super::{AnyError, FINISHED};
use crate::events::{Event, EventKind};
use crate::metadata::Metadata;

#[enum_dispatch]
pub(super) trait WriteEvent {
    fn write(&mut self, event: Event);
}

pub(super) trait DetectBackend
where
    Self: Sized,
{
    fn detect() -> Result<Self, AnyError>;
}

pub struct DebugBackend {
    metadata: Metadata,
}

impl WriteEvent for DebugBackend {
    fn write(&mut self, event: Event) {
        let mir_loc = self.metadata.get(event.mir_loc);
        eprintln!("{:?}: {:?}", mir_loc, event.kind);
    }
}

pub struct LogBackend {
    writer: BufWriter<File>,
}

impl WriteEvent for LogBackend {
    fn write(&mut self, event: Event) {
        bincode::serialize_into(&mut self.writer, &event).unwrap();
    }
}

#[enum_dispatch(WriteEvent)]
pub enum Backend {
    Debug(DebugBackend),
    Log(LogBackend),
}

impl Backend {
    fn write_all(&mut self, rx: Receiver<Event>) {
        for event in rx {
            let done = matches!(event.kind, EventKind::Done);
            self.write(event);
            if done {
                return;
            }
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

impl DetectBackend for DebugBackend {
    fn detect() -> Result<Self, AnyError> {
        let path = {
            let var = "METADATA_FILE";
            env::var_os(var).ok_or_else(|| {
                format!("Instrumentation requires the {var} environment variable be set")
            })?
        };
        // TODO may want to deduplicate this with [`pdg::builder::read_metadata`] in [`Metadata::read`],
        // but that may require adding `color-eyre`/`eyre` as a dependency
        let bytes = fs_err::read(path)?;
        let metadata = bincode::deserialize(&bytes)?;
        Ok(Self { metadata })
    }
}

impl DetectBackend for LogBackend {
    fn detect() -> Result<Self, AnyError> {
        let path = {
            let var = "INSTRUMENT_OUTPUT";
            env::var_os(var).ok_or_else(|| {
                format!("Instrumentation requires the {var} environment variable be set")
            })?
        };
        let append = {
            let var = "INSTRUMENT_OUTPUT_APPEND";
            let append = env::var(var).ok().ok_or_else(|| {
                format!("Instrumentation requires the {var} environment variable be set")
            })?;
            match append.as_str() {
                "true" => true,
                "false" => false,
                _ => return Err(format!("{var} must be 'true' or 'false'").into()),
            }
        };
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .append(append)
            .truncate(!append)
            .open(&path)?;
        let writer = BufWriter::new(file);
        Ok(Self { writer })
    }
}

pub enum BackendKind {
    Debug,
    Log,
}

impl Default for BackendKind {
    fn default() -> Self {
        Self::Debug
    }
}

impl DetectBackend for BackendKind {
    fn detect() -> Result<Self, AnyError> {
        let var = "INSTRUMENT_BACKEND";
        let this = match env::var(var).unwrap_or_default().as_str() {
            "log" => Self::Log,
            "debug" => Self::Debug,
            _ => Self::default(),
        };
        Ok(this)
    }
}

impl Backend {
    pub fn detect_kind(kind: BackendKind) -> Result<Self, AnyError> {
        let this = match kind {
            BackendKind::Debug => Self::Debug(DebugBackend::detect()?),
            BackendKind::Log => Self::Log(LogBackend::detect()?),
        };
        Ok(this)
    }
}

impl DetectBackend for Backend {
    fn detect() -> Result<Self, AnyError> {
        Ok(Self::detect_kind(BackendKind::detect()?)?)
    }
}
