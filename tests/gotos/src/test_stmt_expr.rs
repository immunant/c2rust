use crate::stmt_expr::rust_stmt_expr_func;

use libc::c_int;

pub fn test_stmt_expr_relooper() {
    unsafe {
        assert_eq!(rust_stmt_expr_func(0), 14);
        assert_eq!(rust_stmt_expr_func(1), 13);
        assert_eq!(rust_stmt_expr_func(2), -42);
        assert_eq!(rust_stmt_expr_func(3), 29);
    }
}
