use crate::mir_loc::{Local, MirLocId};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;

pub type Pointer = usize;

#[derive(Serialize, Deserialize, Clone)]
pub struct Event {
    pub mir_loc: MirLocId,
    pub kind: EventKind,
}

impl Event {
    pub fn done() -> Self {
        Self {
            mir_loc: 0,
            kind: EventKind::Done,
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone)]
pub enum EventKind {
    /// A copy from one local to another. This also covers casts such as `&mut
    /// T` to `&T` or `&T` to `*const T` that don't change the type or value of
    /// the pointer.
    CopyPtr(Pointer),

    CopyRef,

    /// Field projection. Used for operations like `_2 = &(*_1).0`. Nested field
    /// accesses like `_4 = &(*_1).x.y.z` are broken into multiple `Node`s, each
    /// covering one level.
    Field(Pointer, u32),

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
    Ret(Pointer),

    /// The pointer appears as the address of a load operation.
    LoadAddr(Pointer),

    /// The pointer appears as the address of a store operation.
    StoreAddr(Pointer),

    StoreAddrTaken(Pointer),

    /// The pointer that appears as the address result of addr_of(Local)
    AddrOfLocal(Pointer, Local),

    /// Casting the pointer to an int
    ToInt(Pointer),

    /// Creating a pointer from an arbitrary int
    FromInt(Pointer),

    LoadValue(Pointer),

    StoreValue(Pointer),

    Offset(Pointer, isize, Pointer),

    Done,
}

impl Debug for EventKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use EventKind::*;
        match *self {
            CopyPtr(ptr) => write!(f, "copy(0x{:x})", ptr),
            Field(ptr, id) => write!(f, "field(0x{:x}, {})", ptr, id),
            Alloc { size, ptr } => {
                write!(f, "malloc({}) -> 0x{:x}", size, ptr)
            }
            Free { ptr } => write!(f, "free(0x{:x})", ptr),
            Realloc {
                old_ptr,
                size,
                new_ptr,
            } => write!(f, "realloc(0x{:x}, {}) -> 0x{:x}", old_ptr, size, new_ptr),
            Ret(ptr) => write!(f, "ret(0x{:x})", ptr),
            Done => write!(f, "done"),
            LoadAddr(ptr) => write!(f, "load(0x{:x})", ptr),
            StoreAddr(ptr) => write!(f, "store(0x{:x})", ptr),
            StoreAddrTaken(ptr) => write!(f, "store(0x{:x})", ptr),
            CopyRef => write!(f, "copy_ref"),
            AddrOfLocal(ptr, _) => write!(f, "addr_of_local = 0x{:x}", ptr),
            ToInt(ptr) => write!(f, "to_int(0x{:x})", ptr),
            FromInt(ptr) => write!(f, "from_int(0x{:x})", ptr),
            LoadValue(ptr) => write!(f, "load_value(0x{:x})", ptr),
            StoreValue(ptr) => write!(f, "store_value(0x{:x})", ptr),
            Offset(ptr, offset, new_ptr) => {
                write!(f, "offset(0x{:x}, {:?}, 0x{:x})", ptr, offset, new_ptr)
            }
        }
    }
}
