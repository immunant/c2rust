#![allow(dead_code)]
use crate::backend::TX;
use crate::events::{Event, EventKind};
use crate::span::SpanId;

pub fn malloc(span: SpanId, size: u64, ptr: usize) {
    TX.send(Event {
        span,
        kind: EventKind::Alloc {
            size: size as usize,
            ptr,
        },
    }).unwrap();
}
pub fn free(span: SpanId, ptr: usize, _ptr: ()) {
    TX.send(Event {
        span,
        kind: EventKind::Free { ptr },
    }).unwrap();
}
pub fn calloc(span: SpanId, nmemb: u64, size: u64, ptr: usize) {
    TX.send(Event {
        span,
        kind: EventKind::Alloc {
            size: (nmemb*size) as usize,
            ptr,
        },
    }).unwrap();
}
pub fn realloc(span: SpanId, old_ptr: usize, size: u64, new_ptr: usize) {
    TX.send(Event {
        span,
        kind: EventKind::Realloc {
            old_ptr,
            size: size as usize,
            new_ptr,
        },
    }).unwrap();
}
pub fn reallocarray(span: SpanId, old_ptr: usize, nmemb: u64, size: u64, new_ptr: usize) {
    TX.send(Event {
        span,
        kind: EventKind::Realloc {
            old_ptr,
            size: (nmemb*size) as usize,
            new_ptr,
        },
    }).unwrap();
}


pub fn ptr_deref(span: SpanId, ptr: usize) {
    TX.send(Event {
        span,
        kind: EventKind::Deref(ptr),
    }).unwrap();
}

pub fn ptr_assign(span: SpanId, ptr: usize) {
    TX.send(Event {
        span,
        kind: EventKind::Assign(ptr),
    }).unwrap();
}

pub fn ptr_arg(span: SpanId, ptr: usize) {
    TX.send(Event {
        span,
        kind: EventKind::Arg(ptr),
    }).unwrap();
}

pub fn ptr_ret(span: SpanId, ptr: usize) {
    TX.send(Event {
        span,
        kind: EventKind::Ret(ptr),
    }).unwrap();
}
