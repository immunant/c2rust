use crate::linking::{rust_l, rust_w};
use std::ffi::c_int;

#[link(name = "test")]
extern "C" {
    fn l() -> c_int;

    fn w() -> c_int;
}

#[test]
pub fn test_linking() {
    let mut ret = unsafe { l() };

    let mut rust_ret = unsafe { rust_l() };

    assert_eq!(ret, rust_ret);
    assert_eq!(ret, 3);
    assert_eq!(rust_ret, 3);

    ret = unsafe { w() };

    rust_ret = unsafe { rust_w() };

    assert_eq!(ret, rust_ret);
    assert_eq!(ret, 4);
    assert_eq!(rust_ret, 4);
}
