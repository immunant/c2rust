pub unsafe fn alias1_good(p: *mut i32) {
    let q = p;
    let r = p;
    *r = 1;
}

pub unsafe fn alias1_bad(p: *mut i32) {
    let q = p;
    let r = p;
    *q = 1;
}


pub fn safe_alias1_good(p: &mut i32) {
    let q = &mut *p;
    let r = &mut *p;
    *r = 1;
}

pub fn safe_alias1_bad(p: &mut i32) {
    let q = &mut *p;
    let r = &mut *p;
    *q = 1;
}
