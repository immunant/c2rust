#![feature(custom_attribute, attr_literals)]

use std::os::raw::c_void;

extern "C" {
    fn free(ptr: *mut c_void);
}

#[derive(Debug,Clone)]
struct RefCounted {
    ref_count: usize,
    data: i32,
}

#[a]
// make sure the old attr gets removed
#[ownership_constraints(le(MOVE, _1))]
#[b]
unsafe fn inc_ref(rc: *mut RefCounted) -> *mut RefCounted {
    (*rc).ref_count += 1;
    rc
}

unsafe fn dec_ref(rc: *mut RefCounted) {
    (*rc).ref_count -= 1;
    if (*rc).ref_count == 0 {
        free(rc as *mut c_void);
    }
}

unsafe fn inc_dec(rc: *mut RefCounted) {
    let rc2 = inc_ref(rc);
    dec_ref(rc2);
}

fn main() {}
