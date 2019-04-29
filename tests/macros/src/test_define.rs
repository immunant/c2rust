extern crate libc;

use define::{TEST_CONST1, TEST_CONST2, TEST_PARENS, rust_reference_define};
use define::{ZSTD_WINDOWLOG_MAX_32, ZSTD_WINDOWLOG_MAX_64, rust_test_zstd};
use define::{rust_fns};
use self::libc::{c_int, c_uint, c_ulong};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn reference_define() -> c_uint;
}

pub fn test_define() {
    let rust_x = unsafe { rust_reference_define() };
    assert_eq!(rust_x, TEST_CONST1 + TEST_CONST2 + TEST_PARENS as c_int);
}

pub fn test_zstd_define() {
    let max = unsafe { rust_test_zstd() } as i32;
    
    assert!(max == ZSTD_WINDOWLOG_MAX_32 || max == ZSTD_WINDOWLOG_MAX_64);
}
