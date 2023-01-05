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
            CopyPtr(ptr) => write!(f, "copy(0x{ptr:x})"),
            Field(ptr, id) => write!(f, "field(0x{ptr:x}, {id})"),
            Alloc { size, ptr } => {
                write!(f, "malloc({size}) -> 0x{ptr:x}")
            }
            Free { ptr } => write!(f, "free(0x{ptr:x})"),
            Realloc {
                old_ptr,
                size,
                new_ptr,
            } => write!(f, "realloc(0x{old_ptr:x}, {size}) -> 0x{new_ptr:x}"),
            Ret(ptr) => write!(f, "ret(0x{ptr:x})"),
            Done => write!(f, "done"),
            BeginFuncBody => write!(f, "begin func body"),
            LoadAddr(ptr) => write!(f, "load(0x{ptr:x})"),
            StoreAddr(ptr) => write!(f, "store(0x{ptr:x})"),
            StoreAddrTaken(ptr) => write!(f, "store(0x{ptr:x})"),
            CopyRef => write!(f, "copy_ref"),
            AddrOfLocal(ptr, _) => write!(f, "addr_of_local = 0x{ptr:x}"),
            ToInt(ptr) => write!(f, "to_int(0x{ptr:x})"),
            FromInt(ptr) => write!(f, "from_int(0x{ptr:x})"),
            LoadValue(ptr) => write!(f, "load_value(0x{ptr:x})"),
            StoreValue(ptr) => write!(f, "store_value(0x{ptr:x})"),
            Offset(ptr, offset, new_ptr) => {
                write!(f, "offset(0x{ptr:x}, {offset:?}, 0x{new_ptr:x})")
            }
        }
    }
}
