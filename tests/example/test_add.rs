extern crate libc;

use add::add as rust_add;
use self::libc::c_uint;

extern "C" {
    #[no_mangle]
    fn add(left: c_uint, right: c_uint) -> c_uint;
}

pub fn test_addition() {
    let sum = unsafe { add(1, 2) };
    let rust_sum = unsafe { rust_add(1, 2) };

    assert_eq!(sum, 3);
    assert_eq!(rust_sum, 3);
}

pub fn test_overflow() {
    let max_uint = c_uint::max_value();
    let sum = unsafe { add(max_uint, 3) };
    let rust_sum = unsafe { rust_add(max_uint, 3) };

    assert_eq!(sum, 2);
    assert_eq!(rust_sum, 2);
}
