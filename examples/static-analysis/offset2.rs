use std::ptr;

pub unsafe fn offset2_good(x: *mut i32, off: isize) {
    let p = x.offset(off);
    let q = x.offset(off);
    *q = 1;
}

pub unsafe fn offset2_bad(x: *mut i32, off: isize) {
    let p = x.offset(off);
    let q = x.offset(off);
    *p = 1;
}
