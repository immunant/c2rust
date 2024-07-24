use crate::events::{Event, EventKind};
use crate::mir_loc::MirLocId;
use crate::runtime::global_runtime::RUNTIME;

// WARNING! Most handlers in this file may be called from a signal handler,
// so they and all their callees should be signal-safe.
// Signal handlers are generally not supposed to call memory allocation
// functions, so those do not need to be signal-safe.

/// A hook function (see [`HOOK_FUNCTIONS`]).
///
/// Instruments 64-bit `c2rust transpile`d `malloc`, which is similar to `libc::malloc`.
pub fn malloc(mir_loc: MirLocId, size: u64, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Alloc {
            size: size as usize,
            ptr,
        },
    });
}

/// A hook function (see [`HOOK_FUNCTIONS`]).
///
/// Instruments 64-bit `c2rust transpile`d `free`, which is similar to `libc::free`.
pub fn free(mir_loc: MirLocId, ptr: usize, _free_ret_val: ()) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Free { ptr },
    });
}

/// A hook function (see [`HOOK_FUNCTIONS`]).
///
/// Instruments 64-bit `c2rust transpile`d `calloc`, which is similar to `libc::calloc`.
pub fn calloc(mir_loc: MirLocId, nmemb: u64, size: u64, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Alloc {
            size: (nmemb * size) as usize,
            ptr,
        },
    });
}

/// A hook function (see [`HOOK_FUNCTIONS`]).
///
/// Instruments 64-bit `c2rust transpile`d `realloc`, which is similar to `libc::realloc`.
pub fn realloc(mir_loc: MirLocId, old_ptr: usize, size: u64, new_ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Free { ptr: old_ptr },
    });
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Alloc {
            size: size as usize,
            ptr: new_ptr,
        },
    });
}

/// A hook function (see [`HOOK_FUNCTIONS`]).
///
/// Instruments 64-bit `c2rust transpile`d `reallocarray`, which is similar to `libc::reallocarray`.
///
/// Note that this is Linux-like-only.
pub fn reallocarray(mir_loc: MirLocId, old_ptr: usize, nmemb: u64, size: u64, new_ptr: usize) {
    realloc(mir_loc, old_ptr, size * nmemb, new_ptr)
}

/// A hook function (see [`HOOK_FUNCTIONS`]).
///
/// Instruments [`pointer::offset`](https://doc.rust-lang.org/std/primitive.pointer.html#method.offset).
///
/// Note that we link to the public docs because `#![feature(intra_doc_pointers)]` is unstable:
///
/// ```shell
/// error[E0658]: linking to associated items of raw pointers is experimental
///   = note: see issue #80896 <https://github.com/rust-lang/rust/issues/80896> for more information
///   = help: add `#![feature(intra_doc_pointers)]` to the crate attributes to enable
///   = note: rustdoc does not allow disambiguating between `*const` and `*mut`, and pointers are unstable until it does
/// ```
pub fn offset(mir_loc: MirLocId, ptr: usize, offset: isize, new_ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Offset(ptr, offset, new_ptr),
    });
}

macro_rules! hook_fn {
    ($name:ident) => {{
        // Ensure it exists and allow rust-analyzer to see through it.
        let _ = self::$name;
        stringify!($name)
    }};
}

/// List of functions we want hooked for the lifetime analysis runtime.
///
/// For functions in [`HOOK_FUNCTIONS`], the tracing passes
/// the return value of the traced function as the last argument to the trace hook for it.
/// See the `if after_call` block in `apply_instrumentation` in `dynamic_instrumentation/src/instrument_memory.rs`.
pub const HOOK_FUNCTIONS: &[&str] = &[
    hook_fn!(malloc),
    hook_fn!(free),
    hook_fn!(calloc),
    hook_fn!(realloc),
    hook_fn!(reallocarray),
    hook_fn!(offset),
];

pub fn ptr_project(mir_loc: MirLocId, ptr: usize, new_ptr: usize, proj_key: u64) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Project(ptr, new_ptr, proj_key),
    });
}

pub fn ptr_copy(mir_loc: MirLocId, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::CopyPtr(ptr as usize),
    });
}

pub fn ptr_contrive(mir_loc: MirLocId, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::FromInt(ptr as usize),
    });
}

pub fn ptr_to_int(mir_loc: MirLocId, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::ToInt(ptr as usize),
    });
}

pub fn addr_of_local(mir_loc: MirLocId, ptr: usize, local: u32) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::AddrOfLocal(ptr, local.into()),
    });
}

pub fn load_value(mir_loc: MirLocId, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::LoadValue(ptr),
    });
}

pub fn store_value(mir_loc: MirLocId, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::StoreValue(ptr),
    });
}

pub fn ptr_ret(mir_loc: MirLocId, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Ret(ptr),
    });
}

pub fn ptr_load(mir_loc: MirLocId, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::LoadAddr(ptr),
    });
}

pub fn ptr_store(mir_loc: MirLocId, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::StoreAddr(ptr),
    });
}

pub fn ptr_store_addr_taken(mir_loc: MirLocId, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::StoreAddrTaken(ptr),
    });
}

pub fn mark_begin_body(mir_loc: MirLocId) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::BeginFuncBody,
    })
}
