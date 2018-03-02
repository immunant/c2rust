extern crate libc;

use no_float_wrapping_neg::no_wrapping_neg as rust_no_wrapping_neg;
use self::libc::c_double;

extern "C" {
    #[no_mangle]
    fn no_wrapping_neg() -> c_double;
}

pub fn test_buffer() {
    unsafe {
        assert_eq!(no_wrapping_neg(), -1.);
        assert_eq!(rust_no_wrapping_neg(), -1.);
    }
}
