#![feature(custom_attribute, attr_literals)]

use std::os::raw::c_void;

extern "C" {
    fn free(ptr: *mut c_void);
}

struct RefCounted {
    #[ownership_static()]
    ref_count: usize,
    #[ownership_static()]
    data: i32,
}

#[a]
// make sure the old attr gets removed
#[b]
#[ownership_constraints(le(MOVE, _1))]
#[ownership_mono("", READ, MOVE)]
unsafe fn inc_ref(rc: *mut RefCounted) -> *mut RefCounted {
    (*rc).ref_count += 1;
    rc
}

#[ownership_constraints(le(WRITE, _0), le(MOVE, _0))]
#[ownership_mono("", MOVE)]
unsafe fn dec_ref(rc: *mut RefCounted) {
    (*rc).ref_count -= 1;
    if (*rc).ref_count == 0 {
        free(rc as *mut c_void);
    }
}

#[ownership_constraints()]
#[ownership_mono("", READ)]
unsafe fn inc_dec(rc: *mut RefCounted) {
    let rc2 = inc_ref(rc);
    dec_ref(rc2);
}

#[ownership_constraints()]
fn main() {}
