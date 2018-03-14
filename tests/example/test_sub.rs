extern crate libc;

use sub::rust_sub;
use self::libc::c_uint;

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn sub(left: c_uint, right: c_uint) -> c_uint;
}

pub fn test_subtraction() {
    let diff = unsafe { sub(5, 2) };
    let rust_diff = unsafe { rust_sub(5, 2) };

    assert_eq!(diff, 3);
    assert_eq!(rust_diff, 3);
}

pub fn test_underflow() {
    let max_uint = c_uint::max_value();
    let diff = unsafe { sub(2, 3) };
    let rust_diff = unsafe { rust_sub(2, 3) };

    assert_eq!(diff, max_uint);
    assert_eq!(rust_diff, max_uint);
}
