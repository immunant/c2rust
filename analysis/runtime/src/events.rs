use std::fmt;
use crate::span;

pub type Pointer = usize;

#[derive(Serialize,Deserialize)]
pub struct Event {
    pub span: usize,
    pub kind: EventKind,
}

impl fmt::Debug for Event {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(span) = span::get(self.span) {
            span.fmt(f)?;
        } else {
            self.span.fmt(f)?;
        }
        write!(f, " {:?}", self.kind)
    }
}

impl Event {
    pub fn done() -> Self {
        Self {
            span: 0,
            kind: EventKind::Done,
        }
    }
}

#[derive(Debug,Serialize,Deserialize)]
pub enum EventKind {
    LibCall(LibFn),
    Deref(Pointer),
    Assign(Pointer),
    Arg(Pointer),
    Done,
}

#[derive(Debug,Serialize,Deserialize)]
pub enum LibFn {
    Malloc { size: u64, result: Pointer },
    Free { ptr: Pointer },
    Calloc { nmemb: u64, size: u64, result: Pointer },
    Realloc { ptr: Pointer, size: u64, result: Pointer },
    ReallocArray { ptr: Pointer, nmemb: u64, size: u64, result: Pointer },
}
