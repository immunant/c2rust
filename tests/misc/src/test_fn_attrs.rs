extern crate libc;

use fn_attrs::{rust_ensure_use, rust_never_inline_nonstatic};

pub fn test_fn_attrs() {
    // There's no way to directly test that a function is inlined or not
    // so instead we're checking the source itself
    let src = include_str!("fn_attrs.rs");

    // Some C99 rules for convenience:
    // * In C99, a function defined inline will never, and a function defined extern inline
    //   will always, emit an externally visible function.
    // * If a non-static function is declared inline, then it must be defined in the same
    //   translation unit. The inline definition that does not use extern is not externally
    //   visible and does not prevent other translation units from defining the same function.
    //   This makes the inline keyword an alternative to static for defining functions inside
    //   header files, which may be included in multiple translation units of the same program.
    // * always_inline implies inline - https://gcc.gnu.org/ml/gcc-help/2007-01/msg00051.html
    //   even if the `inline` keyword isn't present

    assert!(src.contains("#[inline(always)]\nunsafe extern \"C\" fn rust_always_inline_static"));
    assert!(src.contains("#[inline(never)]\nunsafe extern \"C\" fn rust_never_inline_static"));
    assert!(src.contains("#[inline]\nunsafe extern \"C\" fn rust_inline_static"));

    assert!(src.contains("#[inline(always)]\nunsafe extern \"C\" fn rust_always_inline_nonstatic"));
    assert!(src.contains("#[inline(never)]\npub unsafe extern \"C\" fn rust_never_inline_nonstatic"));
    assert!(src.contains("#[inline]\nunsafe extern \"C\" fn rust_inline_nonstatic"));
}
