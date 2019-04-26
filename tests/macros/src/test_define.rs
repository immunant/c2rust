extern crate libc;

use define::{TEST_CONST1, TEST_CONST2, TEST_PARENS, rust_reference_define};
use define::{rust_fns};
use self::libc::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn reference_define() -> c_uint;
}

pub fn test_define() {
    let rust_x = unsafe { rust_reference_define() };
    assert_eq!(rust_x, TEST_CONST1 + TEST_CONST2 + TEST_PARENS as c_int);
}
