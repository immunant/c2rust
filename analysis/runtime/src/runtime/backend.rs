use enum_dispatch::enum_dispatch;
use fs_err::{File, OpenOptions};
use std::fmt::Debug;
use std::io::BufWriter;
use std::sync::mpsc::Receiver;

use bincode;

use super::{AnyError, FINISHED};
use crate::events::{Event, EventKind};
use crate::metadata::Metadata;
use crate::parse::{self, AsStr, GetChoices};

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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BackendKind {
    Debug,
    Log,
}

impl AsStr for BackendKind {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Debug => "debug",
            Self::Log => "log",
        }
    }
}

impl GetChoices for BackendKind {
    fn choices() -> &'static [Self] {
        &[Self::Debug, Self::Log]
    }
}

impl Default for BackendKind {
    fn default() -> Self {
        Self::Debug
    }
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
    debug: DebugBackend,
}

impl WriteEvent for LogBackend {
    fn write(&mut self, event: Event) {
        bincode::serialize_into(&mut self.writer, &event).unwrap();
        self.debug.write(event);
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
        let path = parse::env::path("METADATA_FILE")?;
        // TODO may want to deduplicate this with [`pdg::builder::read_metadata`] in [`Metadata::read`],
        // but that may require adding `color-eyre`/`eyre` as a dependency
        let bytes = fs_err::read(path)?;
        let metadata = Metadata::read(&bytes)?;
        Ok(Self { metadata })
    }
}

impl DetectBackend for LogBackend {
    fn detect() -> Result<Self, AnyError> {
        let path = parse::env::path("INSTRUMENT_OUTPUT")?;
        let append: bool = *parse::env::one_of("INSTRUMENT_OUTPUT_APPEND")?;
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .append(append)
            .truncate(!append)
            .open(&path)?;
        let writer = BufWriter::new(file);
        let debug = DebugBackend::detect()?;
        Ok(Self { writer, debug })
    }
}

impl DetectBackend for BackendKind {
    fn detect() -> Result<Self, AnyError> {
        Ok(parse::env::one_of("INSTRUMENT_BACKEND").cloned()?)
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
        Self::detect_kind(BackendKind::detect()?)
    }
}
