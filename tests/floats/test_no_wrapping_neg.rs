extern crate libc;

use no_float_wrapping_neg::{rust_no_wrapping_neg, rust_float_inc_dec, rust_double_inc_dec};
use self::libc::{c_double, c_float};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn no_wrapping_neg() -> c_double;
    #[no_mangle]
    fn float_inc_dec() -> c_float;
    #[no_mangle]
    fn double_inc_dec() -> c_double;
}

pub fn test_buffer() {
    unsafe {
        assert_eq!(no_wrapping_neg(), -1.);
        assert_eq!(rust_no_wrapping_neg(), -1.);
    }
}

pub fn test_inc_dec_op() {
    unsafe {
        assert_eq!(float_inc_dec(), -0.79999995);
        assert_eq!(rust_float_inc_dec(), -0.79999995);
        assert_eq!(double_inc_dec(), -0.7999999999999998);
        assert_eq!(rust_double_inc_dec(), -0.7999999999999998);
    }
}
