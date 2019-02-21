extern crate libc;

use sections::*;
use self::libc::c_uint;

pub fn test_sectioned_statics() {
    unsafe {
        assert_eq!(rust_section_me, c_uint::max_value());
        assert_eq!(rust_section_me2, 0i32);
        assert_eq!(rust_section_me3, 3u32);
        assert_eq!(rust_section_me4, 2u32);
        assert_eq!(rust_section_me5, 2u32);
        assert_eq!(rust_section_foo_b_field.a, 1);
        assert_eq!(rust_section_foo_b_field.b, -1);
        assert_eq!(rust_section_foo_b_field.c, 1.2);
        assert_eq!(rust_section_num_params, 2);
        assert!(rust_if_expr == 30 || rust_if_expr == 31);

        // There's not really a way to test the function scoped static
        // directly since it's (rightly) private. But this does prove
        // that the previously uncompilable static is now being initialized
        let ptr_deref = unsafe {
            *(rust_fn_scoped_static_init() as *const c_uint)
        };
        assert_eq!(ptr_deref, c_uint::max_value());

        rust_use_sectioned_array();
    }
}
