/// Check that `extern "rust-intrinsic"` (which can be generic) foreign `fn`s
/// like [`std::mem::transmute`] don't crash `c2rust-analyze`.
///
/// They currently do (in [`Instance::mono`] where there are generic args),
/// which is why this is `#[cfg]`ed out for now.
#[cfg(any())]
pub unsafe fn f(x: *const u8) -> *const i8 {
    std::mem::transmute(x)
}
