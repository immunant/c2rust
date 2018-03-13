extern crate libc;

use no_float_wrapping_neg::rust_no_wrapping_neg;
use self::libc::c_double;

#[link(name = "test")]
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
