use crate::events::{Event, EventKind};
use crate::mir_loc::MirLocId;
use crate::runtime::global_runtime::RUNTIME;

pub fn malloc(mir_loc: MirLocId, size: u64, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Alloc {
            size: size as usize,
            ptr,
        },
    });
}

pub fn free(mir_loc: MirLocId, ptr: usize, _ptr: ()) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Free { ptr },
    });
}

pub fn calloc(mir_loc: MirLocId, nmemb: u64, size: u64, ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Alloc {
            size: (nmemb * size) as usize,
            ptr,
        },
    });
}

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

pub fn reallocarray(mir_loc: MirLocId, old_ptr: usize, nmemb: u64, size: u64, new_ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Realloc {
            old_ptr,
            size: (nmemb * size) as usize,
            new_ptr,
        },
    });
}

pub fn offset(mir_loc: MirLocId, ptr: usize, offset: isize, new_ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Offset(ptr, offset, new_ptr),
    });
}

pub fn ptr_field(mir_loc: MirLocId, ptr: usize, field_id: u32) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::Field(ptr, field_id),
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

pub fn ref_copy(mir_loc: MirLocId, _dest_local_ptr: usize) {
    RUNTIME.send_event(Event {
        mir_loc,
        kind: EventKind::CopyRef,
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
