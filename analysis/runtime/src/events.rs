use crate::mir_loc::{Local, MirLocId};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;

pub type Pointer = usize;

#[derive(Serialize, Deserialize, Debug)]
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

    /// Projection. Used for operations like `_2 = &(*_1).0`.
    /// The third value is a "projection index" that points to an element
    /// of the projections data structure in the metadata. It is used to
    /// disambiguate between different projections with the same pointer,
    /// e.g., `(*p).x` and `(*p).x.a` where `a` is at offset 0.
    Project(Pointer, Pointer, usize),

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

    /// An address-taken local is assigned to, which is semantically the
    /// same as [`StoreAddr`](Self::StoreAddr), but needs to be distinguished because
    /// storing to an address-taken local does not imply write permissions
    /// are necessary for its underlying address.
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

    /// Marks the start of events in a new function body.
    /// Used to distinguish address-taken locals that are treated
    /// as copies and ones that aren't; all but the first [`AddrOfLocal`](Self::AddrOfLocal)
    /// events after a [`BeginFuncBody`](Self::BeginFuncBody) event are treated as copies.
    BeginFuncBody,

    Done,
}

impl Debug for EventKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use EventKind::*;
        match *self {
            CopyPtr(ptr) => write!(f, "copy(0x{:x})", ptr),
            Project(ptr, new_ptr, idx) => {
                write!(f, "project(0x{:x}, 0x{:x}, [{}])", ptr, new_ptr, idx)
            }
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
            BeginFuncBody => write!(f, "begin func body"),
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
