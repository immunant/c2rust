use crate::typedef::{int_ptr, my_int, rust_entry};

use std::ffi::c_int;

#[link(name = "test")]
extern "C" {
    fn entry() -> c_int;
}

#[test]
pub fn test_typedef() {
    let ret = unsafe { entry() };
    let rust_ret = unsafe { rust_entry() };
    assert_eq!(ret, 0);
    assert_eq!(rust_ret, 0);

    let mut rust_var: my_int = 5;
    let mut c_var: c_int = 5;
    assert_eq!(rust_var, c_var);

    let rptr_var: int_ptr = &mut rust_var as *mut my_int;
    let cptr_var: int_ptr = &mut c_var as *mut c_int;

    unsafe {
        assert_eq!(*rptr_var, *cptr_var);
    }
}
