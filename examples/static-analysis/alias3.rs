use std::ptr;

pub unsafe fn alias3_copy_bad1(x: *mut i32) {
    let p = x;
    let q = x;
    *q = *p;
}

pub unsafe fn alias3_copy_bad2(x: *mut i32) {
    let p = x;
    let q = x;
    *p = *q;
}

pub unsafe fn alias3_addr_of_bad1(x: *mut i32) {
    let p = ptr::addr_of_mut!(*x);
    let q = ptr::addr_of_mut!(*x);
    *q = *p;
}

pub unsafe fn alias3_addr_of_bad2(x: *mut i32) {
    let p = ptr::addr_of_mut!(*x);
    let q = ptr::addr_of_mut!(*x);
    *p = *q;
}
