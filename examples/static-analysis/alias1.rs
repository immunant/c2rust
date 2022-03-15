use std::ptr;

pub unsafe fn alias1_good() {
    let mut x = 0;
    let p = ptr::addr_of_mut!(x);
    let q = ptr::addr_of_mut!(x);
    *q = 1;
}

pub unsafe fn alias1_bad() {
    let mut x = 0;
    let p = ptr::addr_of_mut!(x);
    let q = ptr::addr_of_mut!(x);
    *p = 1;
}


pub fn safe_alias1_good() {
    let mut x = 0;
    let p = &mut x;
    let q = &mut x;
    *q = 1;
}

pub fn safe_alias1_bad() {
    let mut x = 0;
    let p = &mut x;
    let q = &mut x;
    *p = 1;
}
