#![allow(dead_code)]
use crate::backend::TX;
use crate::events::{Event, EventKind};
use crate::mir_loc::MirLocId;

pub fn malloc(mir_loc: MirLocId, size: u64, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Alloc {
            size: size as usize,
            ptr,
        },
    }).unwrap();
}
pub fn free(mir_loc: MirLocId, ptr: usize, _ptr: ()) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Free { ptr },
    }).unwrap();
}
pub fn calloc(mir_loc: MirLocId, nmemb: u64, size: u64, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Alloc {
            size: (nmemb*size) as usize,
            ptr,
        },
    }).unwrap();
}
pub fn realloc(mir_loc: MirLocId, old_ptr: usize, size: u64, new_ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Realloc {
            old_ptr,
            size: size as usize,
            new_ptr,
        },
    }).unwrap();
}
pub fn reallocarray(mir_loc: MirLocId, old_ptr: usize, nmemb: u64, size: u64, new_ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Realloc {
            old_ptr,
            size: (nmemb*size) as usize,
            new_ptr,
        },
    }).unwrap();
}


pub fn ptr_field(mir_loc: MirLocId, ptr: usize, field_id: u32) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Field(ptr, field_id),
    }).unwrap();
}

pub fn ptr_copy(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Copy(ptr),
    }).unwrap();
}

pub fn ptr_arg(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Arg(ptr),
    }).unwrap();
}

pub fn ptr_ret(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::Ret(ptr),
    }).unwrap();
}

pub fn ptr_load(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::LoadAddr(ptr),
    }).unwrap();
}

pub fn ptr_store(mir_loc: MirLocId, ptr: usize) {
    TX.send(Event {
        mir_loc,
        kind: EventKind::StoreAddr(ptr),
    }).unwrap();
}
