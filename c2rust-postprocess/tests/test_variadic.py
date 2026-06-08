from postprocess.transforms.variadic import rewrite_variadic_compat


def test_rewrite_variadic_compat_replaces_va_list_impl_and_as_va_list() -> None:
    original = """
pub unsafe extern "C" fn demo(mut c2rust_args: ...) {
    let mut ap: ::core::ffi::VaListImpl;
    ap = c2rust_args.clone();
    sink(ap.as_va_list());
}
"""

    rewritten, changed = rewrite_variadic_compat(original)

    assert changed is True
    assert "::core::ffi::VaListImpl" not in rewritten
    assert "::core::ffi::VaList" in rewritten
    assert ".as_va_list()" not in rewritten
    assert "ap.clone()" in rewritten
