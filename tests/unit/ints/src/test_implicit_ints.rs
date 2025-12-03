//! xfail

use crate::implicit_int::{identity as rust_identity, implicit_int as rust_implicit_int};
use std::ffi::{c_int, c_uint};

extern "C" {
    fn identity(_: c_int) -> c_int;

    fn implicit_int();
}

#[test]
pub fn test_identity() {
    unsafe {
        assert_eq!(identity(1), 1);
        assert_eq!(rust_identity(1), 1);
    }
}

#[test]
pub fn test_implicit_int() {
    unsafe {
        implicit_int();
        rust_implicit_int();
    }
}
