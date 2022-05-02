use std::ptr;

pub unsafe fn offset1_const(x: *mut i32) -> i32 {
    *x.offset(1)
}

pub unsafe fn offset1_unknown(x: *mut i32, off: isize) -> i32 {
    *x.offset(off)
}

/*
pub unsafe fn offset1_usize(x: *mut i32, off: usize) -> i32 {
    *x.offset(off as isize)
}
*/

pub unsafe fn offset1_immut(x: *const i32, off: isize) -> i32 {
    *x.offset(off)
}
