extern crate libc;

use stmt_expr::rust_stmt_expr_func;

use self::libc::c_int;

pub fn test_stmt_expr_relooper() {
    unsafe {
        assert_eq!(rust_stmt_expr_func(0), 13);
        assert_eq!(rust_stmt_expr_func(1), 12);
        assert_eq!(rust_stmt_expr_func(2), 26);
        assert_eq!(rust_stmt_expr_func(3), 28);
    }
}
