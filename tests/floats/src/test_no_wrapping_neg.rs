use crate::no_float_wrapping_neg::{rust_double_inc_dec, rust_float_inc_dec, rust_no_wrapping_neg};
use std::ffi::{c_double, c_float};

#[link(name = "test")]
extern "C" {
    fn no_wrapping_neg() -> c_double;
    fn float_inc_dec() -> c_float;
    fn double_inc_dec() -> c_double;
}

#[test]
pub fn test_buffer() {
    unsafe {
        assert_eq!(no_wrapping_neg(), -1.);
        assert_eq!(rust_no_wrapping_neg(), -1.);
    }
}

#[test]
pub fn test_inc_dec_op() {
    unsafe {
        assert_eq!(float_inc_dec(), -0.79999995);
        assert_eq!(rust_float_inc_dec(), -0.79999995);
        assert_eq!(double_inc_dec(), -0.7999999999999998);
        assert_eq!(rust_double_inc_dec(), -0.7999999999999998);
    }
}
