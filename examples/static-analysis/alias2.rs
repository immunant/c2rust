use std::ptr;

pub unsafe fn alias2_copy_good(x: *mut i32) {
    let p = x;
    let q = x;
    *q = 1;
}

pub unsafe fn alias2_addr_of_good(x: *mut i32) {
    let p = ptr::addr_of_mut!(*x);
    let q = ptr::addr_of_mut!(*x);
    *q = 1;
}

pub unsafe fn alias2_copy_bad(x: *mut i32) {
    let p = x;
    let q = x;
    *p = 1;
}

pub unsafe fn alias2_addr_of_bad(x: *mut i32) {
    let p = ptr::addr_of_mut!(*x);
    let q = ptr::addr_of_mut!(*x);
    *p = 1;
}


pub unsafe fn safe_alias2_copy_good(x: &mut i32) {
    let p = x;
    let q = x;
    *q = 1;
}

pub unsafe fn safe_alias2_addr_of_good(x: &mut i32) {
    let p = &mut *x;
    let q = &mut *x;
    *q = 1;
}

pub unsafe fn safe_alias2_copy_bad(x: &mut i32) {
    let p = x;
    let q = x;
    *p = 1;
}

pub unsafe fn safe_alias2_addr_of_bad(x: &mut i32) {
    let p = &mut *x;
    let q = &mut *x;
    *p = 1;
}
