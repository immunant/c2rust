use enum_dispatch::enum_dispatch;
use fs_err::File;
use std::env;
use std::io::BufWriter;
use std::sync::mpsc::Receiver;

use bincode;

use super::{AnyError, FINISHED};
use crate::events::{Event, EventKind};
use crate::metadata::{IWithMetadata, Metadata};

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
        // TODO may want to deduplicate this with [`pdg::builder::read_metadata`] in [`Metadata::read`],
        // but that may require adding `color-eyre`/`eyre` as a dependency
        let bytes = fs_err::read(path)?;
        let metadata = bincode::deserialize(&bytes)?;
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
