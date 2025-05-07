#![feature(rustc_private)]
#![feature(c_variadic)]

pub unsafe extern "C" fn testing() {
    let mut x = 10i32;
    let mut ptr = &mut x as *mut i32;
    let ref mut fresh1 = ptr;
    *fresh1 = &mut x as *mut i32;
    let fresh2 = fresh1;
    **fresh2 = 0;
}

fn main() {
    unsafe { testing() }
}
