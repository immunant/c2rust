use std::fmt;
use crate::span::{self, SpanId};

pub type Pointer = usize;

#[derive(Serialize,Deserialize)]
pub struct Event {
    pub span: SpanId,
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

#[derive(Serialize,Deserialize,Copy,Clone)]
pub enum EventKind {
    Alloc {
        size: usize,
        ptr: Pointer,
    },
    Free {
        ptr: Pointer,
    },
    Realloc {
        old_ptr: Pointer,
        size: usize,
        new_ptr: Pointer,
    },
    Deref(Pointer),
    Assign(Pointer),
    Arg(Pointer),
    Done,
}

impl fmt::Debug for EventKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            EventKind::Alloc { size, ptr } => {
                write!(f, "malloc({}) -> {:p}", size, ptr as *const u8)
            }
            EventKind::Free { ptr } => write!(f, "free({:p})", ptr as *const u8),
            EventKind::Realloc { old_ptr, size, new_ptr } => write!(
                f,
                "realloc({:p}, {}) -> {:p}",
                old_ptr as *const u8, size, new_ptr as *const u8
            ),
            EventKind::Deref(ptr) => write!(f, "deref({:p})", ptr as *const u8),
            EventKind::Assign(ptr) => write!(f, "assign({:p})", ptr as *const u8),
            EventKind::Arg(ptr) => write!(f, "arg({:p})", ptr as *const u8),
            EventKind::Done => write!(f, "done"),
        }
    }
}
