extern crate libc;

use fn_attrs::{rust_ensure_use, rust_always_inline_pub, rust_never_inline_pub};

pub fn test_fn_attrs() {
    // There's no way to directly test that a function is inlined or not
    // so instead we're checking the source itself
    let src = include_str!("fn_attrs.rs");

    assert!(src.contains("#[inline(always)]\nunsafe extern \"C\" fn rust_always_inline_foo"));
    assert!(src.contains("#[inline(never)]\nunsafe extern \"C\" fn rust_never_inline_foo"));

    assert!(src.contains("#[inline(always)]\npub unsafe extern \"C\" fn rust_always_inline_pub"));
    assert!(src.contains("#[inline(never)]\npub unsafe extern \"C\" fn rust_never_inline_pub"));
}
