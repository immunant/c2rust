use crate::define::{rust_fns, rust_stmt_expr_inc};
use crate::define::{rust_reference_define, TEST_CONST1, TEST_CONST2, TEST_PARENS};
use crate::define::{rust_test_zstd, ZSTD_WINDOWLOG_MAX_32, ZSTD_WINDOWLOG_MAX_64};
use std::ffi::{c_int, c_uint, c_ulong};

#[link(name = "test")]
extern "C" {
    fn reference_define() -> c_uint;
}

#[test]
pub fn test_define() {
    let rust_x = unsafe { rust_reference_define() };
    assert_eq!(rust_x, TEST_CONST1 + TEST_CONST2 + TEST_PARENS as c_int);
}

#[test]
pub fn test_zstd_define() {
    let max = unsafe { rust_test_zstd() } as i32;

    assert!(max == ZSTD_WINDOWLOG_MAX_32 || max == ZSTD_WINDOWLOG_MAX_64);
}

#[test]
pub fn test_macro_stmt_expr() {
    let ret = unsafe { rust_stmt_expr_inc() };

    assert_eq!(ret, 2);
}
