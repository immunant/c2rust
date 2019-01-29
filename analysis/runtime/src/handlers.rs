#![allow(dead_code)]
use crate::backend::{TX, Event, EventKind, LibFn};

pub fn malloc(span: usize, size: u64, result: usize) {
    TX.send(Event {
        span,
        kind: EventKind::LibCall(
            LibFn::Malloc { size, result }
        ),
    }).unwrap();
}
pub fn free(span: usize, ptr: usize, _result: ()) {
    TX.send(Event {
        span,
        kind: EventKind::LibCall(
            LibFn::Free { ptr }
        ),
    }).unwrap();
}
pub fn calloc(span: usize, nmemb: u64, size: u64, result: usize) {
    TX.send(Event {
        span,
        kind: EventKind::LibCall(
            LibFn::Calloc { nmemb, size, result }
        ),
    }).unwrap();
}
pub fn realloc(span: usize, ptr: usize, size: u64, result: usize) {
    TX.send(Event {
        span,
        kind: EventKind::LibCall(
            LibFn::Realloc { ptr, size, result }
        ),
    }).unwrap();
}
pub fn reallocarray(span: usize, ptr: usize, nmemb: u64, size: u64, result: usize) {
    TX.send(Event {
        span,
        kind: EventKind::LibCall(
            LibFn::ReallocArray { ptr, nmemb, size, result }
        ),
    }).unwrap();
}


pub fn ptr_deref(span: usize, ptr: usize) {
    TX.send(Event {
        span,
        kind: EventKind::Deref(ptr),
    }).unwrap();
}

pub fn ptr_assign(span: usize, ptr: usize) {
    TX.send(Event {
        span,
        kind: EventKind::Assign(ptr),
    }).unwrap();
}

pub fn ptr_arg(span: usize, ptr: usize) {
    TX.send(Event {
        span,
        kind: EventKind::Arg(ptr),
    }).unwrap();
}
