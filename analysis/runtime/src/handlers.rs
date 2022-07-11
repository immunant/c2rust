use crate::backend::TX;
use crate::events::{Event, EventKind};
use crate::mir_loc::MirLocId;

/// A hook function (see [`HOOK_FUNCTIONS`]).
pub fn malloc(mir_loc: MirLocId, size: u64, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Alloc {
            size: size as usize,
            ptr,
        },
    })
    .unwrap();
}

/// A hook function (see [`HOOK_FUNCTIONS`]).
pub fn free(mir_loc: MirLocId, ptr: usize, _free_ret_val: ()) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Free { ptr },
    })
    .unwrap();
}

/// A hook function (see [`HOOK_FUNCTIONS`]).
pub fn calloc(mir_loc: MirLocId, nmemb: u64, size: u64, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Alloc {
            size: (nmemb * size) as usize,
            ptr,
        },
    })
    .unwrap();
}

/// A hook function (see [`HOOK_FUNCTIONS`]).
pub fn realloc(mir_loc: MirLocId, old_ptr: usize, size: u64, new_ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Free { ptr: old_ptr },
    })
    .unwrap();
    TX.send(Event {
        mir_loc,
        kind: EventKind::Alloc {
            size: size as usize,
            ptr: new_ptr,
        },
    })
    .unwrap();
}

/// A hook function (see [`HOOK_FUNCTIONS`]).
pub fn reallocarray(mir_loc: MirLocId, old_ptr: usize, nmemb: u64, size: u64, new_ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Realloc {
            old_ptr,
            size: (nmemb * size) as usize,
            new_ptr,
        },
    })
    .unwrap();
}

/// A hook function (see [`HOOK_FUNCTIONS`]).
pub fn offset(mir_loc: MirLocId, ptr: usize, offset: isize, new_ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Offset(ptr, offset, new_ptr),
    })
    .unwrap();
}

type Pointer = *const ();

/// Ensure the function exists both as a handler above and as a function in transpiled code.
/// Usually that means either [`std`], [`core`], or [`libc`].
/// 
/// Type-checking is not performed, as that's too complicated,
/// but this at least ensures names aren't changed and it's clear what needs to match up with what.
/// 
/// Note that uses of [`hook_fn`] use [`libc`],
/// even though the transpiled versions are slightly different from [`libc`].
/// For example, they may use [`libc::size_t`] ([`usize`]) instead of [`libc::c_ulong`] ([`u64`]).
/// We're just checking function names, though, so this is okay.
/// 
/// This also allows IDEs like rust-analyzer and IntelliJ Rust to see both the hooked function and it's handler.
macro_rules! hook_fn {
    ($module:ident::$name:ident) => {{
        // Ensure it exists and allow rust-analyzer to see through it.
        let _ = self::$name;
        let _ = $module::$name;
        stringify!($name)
    }}
}

/// List of functions we want hooked for the lifetime analyis runtime.
/// 
/// For functions in [`HOOK_FUNCTIONS`], the tracing passes 
/// the return value of the traced function as the last argument to the trace hook for it.
/// See the `if after_call` block in `apply_instrumentation` in `dynamic_instrumentation/src/instrument_memory.rs`.
pub const HOOK_FUNCTIONS: &[&str] = &[
    hook_fn!(libc::malloc),
    hook_fn!(libc::free),
    hook_fn!(libc::calloc),
    hook_fn!(libc::realloc),
    hook_fn!(libc::reallocarray),
    hook_fn!(Pointer::offset),
];

pub fn ptr_field(mir_loc: MirLocId, ptr: usize, field_id: u32) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Field(ptr, field_id),
    })
    .unwrap();
}

pub fn ptr_copy(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::CopyPtr(ptr as usize),
    })
    .unwrap();
}

pub fn ptr_contrive(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::FromInt(ptr as usize),
    })
    .unwrap();
}

pub fn ptr_to_int(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::ToInt(ptr as usize),
    })
    .unwrap();
}

pub fn addr_of_local(mir_loc: MirLocId, ptr: usize, local: u32) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::AddrOfLocal(ptr, local.into()),
    })
    .unwrap();
}

pub fn ref_copy(mir_loc: MirLocId, _dest_local_ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::CopyRef,
    })
    .unwrap();
}

pub fn load_value(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::LoadValue(ptr),
    })
    .unwrap();
}

pub fn store_value(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::StoreValue(ptr),
    })
    .unwrap();
}

pub fn ptr_ret(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Ret(ptr),
    })
    .unwrap();
}

pub fn ptr_load(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::LoadAddr(ptr),
    })
    .unwrap();
}

pub fn ptr_store(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::StoreAddr(ptr),
    })
    .unwrap();
}
