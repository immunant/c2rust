#[cfg(not(target_os = "macos"))]
use crate::attributes::{rust_no_attrs, rust_used_static, rust_used_static2, rust_used_static3};
use crate::sections::*;
use std::ffi::c_uint;

#[test]
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
        let ptr_deref = unsafe { *(rust_fn_scoped_static_init() as *const c_uint) };
        assert_eq!(ptr_deref, c_uint::max_value() - 1);
        assert_eq!(rust_section_me, c_uint::max_value() - 1);

        rust_use_sectioned_array();
    }
}

#[test]
pub fn test_sectioned_used_static() {
    if cfg!(not(target_os = "macos")) {
        // This static variable is private and unused (but with the used attribute)
        // so there's no way to directly test it was generated except through looking
        // directly at the source file
        let src = include_str!("attributes.rs");

        let mut lines = src.lines().collect::<Vec<_>>();

        // Remove the c2rust::src_loc annotation, which is only produced if
        // --reorganize-definitions is enabled.
        lines.retain(|x| !x.contains("#[c2rust::src_loc"));
        let src = lines.join("\n");

        let pos = lines
            .iter()
            .position(|&x| {
                x == "static mut rust_used_static4: ::core::ffi::c_int = 1 as ::core::ffi::c_int;"
            })
            .expect("Did not find expected static string in source");
        // The ordering of these attributes is not stable between LLVM versions
        assert!(
            (lines[pos - 1] == "#[used]" && lines[pos - 2] == "#[link_section = \"barz\"]")
                || (lines[pos - 2] == "#[used]" && lines[pos - 1] == "#[link_section = \"barz\"]")
        );

        // This static is pub, but we want to ensure it has attributes applied
        assert!(src.contains(
            r#"
#[link_section = "fb"]
pub static mut rust_initialized_extern: ::core::ffi::c_int = 1 as ::core::ffi::c_int;
"#
            .trim(),
        ));
        // This static is pub only with --reorganize-definitions
        let aliased_static_syntax = |public| {
            format!(
                r#"
extern "C" {{
    #[link_name = "no_attrs"]
    {}static mut rust_aliased_static: ::core::ffi::c_int;
"#,
                public
            )
        };
        assert!(
            src.contains(aliased_static_syntax("").trim())
                || src.contains(aliased_static_syntax("pub ").trim())
        )
    }
}
