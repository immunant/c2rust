use crate::fn_attrs::{rust_ensure_use, rust_inline_extern, rust_noinline_nonstatic};

#[test]
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
    // * gnu_inline instead applies gnu89 rules. extern inline will not emit an externally
    //   visible function.

    // static __attribute__((always_inline)) void always_inline_static(void) {}
    // static __attribute__((__noinline__)) void noinline_static(void) {}
    // static void inline inline_static(void) {}
    assert!(src.contains("#[inline(always)]\nunsafe extern \"C\" fn rust_always_inline_static"));
    assert!(src.contains("#[inline(never)]\nunsafe extern \"C\" fn rust_noinline_static"));
    assert!(src.contains("#[inline]\nunsafe extern \"C\" fn rust_inline_static"));
    assert!(src.contains("#[inline]\nunsafe extern \"C\" fn rust_alt_kw_inline_static"));
    assert!(src.contains("#[inline]\nunsafe extern \"C\" fn rust_gnu_inline_static"));
    assert!(src.contains("#[cold]\nunsafe extern \"C\" fn rust_cold_used_attrs"));

    // __attribute__((__always_inline__)) void always_inline_nonstatic(void) {}
    // __attribute__((noinline)) void noinline_nonstatic(void) {}
    // void inline inline_nonstatic(void) {}
    assert!(src.contains("#[inline(always)]\nunsafe extern \"C\" fn rust_always_inline_nonstatic"));
    assert!(src.contains("#[inline(never)]\npub unsafe extern \"C\" fn rust_noinline_nonstatic"));
    assert!(src.contains("#[inline]\nunsafe extern \"C\" fn rust_inline_nonstatic"));
    assert!(src.contains("#[inline]\nunsafe extern \"C\" fn rust_alt_kw_inline_nonstatic"));
    assert!(src.contains("#[inline]\nunsafe extern \"C\" fn rust_gnu_inline_nonstatic"));

    // extern void inline inline_extern(void) {}
    // extern void inline __attribute__((always_inline)) always_inline_extern(void) {}
    // extern void inline __attribute__((__gnu_inline__)) gnu_inline_extern(void) {}
    // extern void inline __attribute__((gnu_inline, always_inline)) always_inline_gnu_inline_extern(void) {}
    assert!(src.contains(
        "#[inline]\n#[linkage = \"external\"]\npub unsafe extern \"C\" fn rust_inline_extern"
    ));
    assert!(src.contains("#[inline]\n#[linkage = \"external\"]\npub unsafe extern \"C\" fn rust_alt_kw_inline_extern"));
    assert!(src.contains("#[inline(always)]\npub unsafe extern \"C\" fn rust_always_inline_extern"));
    assert!(src.contains("#[inline]\nunsafe extern \"C\" fn rust_gnu_inline_extern"));
    assert!(src.contains(
        "#[inline(always)]\nunsafe extern \"C\" fn rust_always_inline_gnu_inline_extern"
    ));
    assert!(src.contains(
        "#[inline]\nunsafe extern \"C\" fn rust_gnu_inline_non_canonical_definition_extern"
    ));

    if cfg!(not(target_os = "macos")) {
        // aliased_fn is aliased to the inline_extern function
        assert!(src.contains(
            "extern \"C\" {\n    #[link_name = \"inline_extern\"]\n    fn aliased_fn();"
        ));
    }
}
