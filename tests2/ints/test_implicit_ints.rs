//! xfail
extern crate libc;

use implicit_int::{identity as rust_identity, implicit_int as rust_implicit_int};
use self::libc::{c_int, c_uint};

extern "C" {
    #[no_mangle]
    fn identity(_: c_int) -> c_int;

    #[no_mangle]
    fn implicit_int();
}

pub fn test_identity() {
    unsafe {
        assert_eq!(identity(1), 1);
        assert_eq!(rust_identity(1), 1);
    }
}

pub fn test_implicit_int() {
    unsafe {
        implicit_int();
        rust_implicit_int();
    }
}
