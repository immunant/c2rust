extern crate libc;

use attributes::{rust_used_static, rust_used_static2, rust_used_static3, rust_no_attrs};
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

pub fn test_sectioned_used_static() {
    // This static variable is private and unused (but with the used attribute)
    // so there's no way to directly test it was generated except through looking
    // directly at the source file
    let src = include_str!("attributes.rs");

    assert!(src.contains("#[link_section = \"barz\"]\n#[used]\nstatic mut rust_used_static4: libc::c_int = 1i32;"));

    // This static is pub, but we want to ensure it has attributes applied
    assert!(src.contains("#[link_section = \"fb\"]\npub static mut rust_initialized_extern: libc::c_int = 1i32;"));
}
