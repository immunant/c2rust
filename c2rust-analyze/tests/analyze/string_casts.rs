pub fn cast_ptr_to_ptr(s: *const u8) {
    s as *const core::ffi::c_char;
}

pub fn deep_cast_ptr_to_ptr(x: *const *const u8) {
    x as *const *const i8;
}

/// For the below disabled (`#[cfg(any())]`ed) tests, they currently crash in the rewriter
/// due to it not being able to handle implicitly inserted `&raw` MIR statements yet.
/// Thus, they also have `*_explicit` versions where
/// a `std::ptr::addr_of!` is used to make the `&raw` explicit.
///
/// Also note that `addr_of!` (with a `use std::ptr::addr_of`)
/// and `::core::ptr::addr_of!` don't work either,
/// though `std::ptr::addr_of`, `::std::ptr::addr_of!`,
/// and `core::ptr::addr_of!` do work.

#[cfg(any())]
pub fn cast_array_to_ptr(s: &[u8; 1]) {
    s as *const u8;
}

pub fn cast_array_to_ptr_explicit(s: &[u8; 1]) {
    std::ptr::addr_of!(*s) as *const u8;
}

#[cfg(any())]
pub fn cast_from_literal() {
    b"\0" as *const u8 as *const core::ffi::c_char;
}

pub fn cast_from_literal_explicit() {
    std::ptr::addr_of!(*b"\0") as *const u8 as *const core::ffi::c_char;
}

/// [`PointerCast::ReifyFnPointer`]
pub fn cast_fn_item_to_fn_ptr() {
    fn f(x: u8) -> i8 {
        x as i8
    }
    f as fn(u8) -> i8;
}

/// [`PointerCast::UnsafeFnPointer`]
/// 
/// ```shell
/// thread 'rustc' panicked at 'not yet implemented', c2rust-analyze/src/labeled_ty.rs:372:17
/// ```
#[cfg(any())]
pub fn cast_fn_ptr_to_unsafe_fn_ptr(f: fn(u8) -> i8) {
    f as unsafe fn(u8) -> i8;
}

/// [`PointerCast::ClosureFnPointer`]
/// Unhandled very early on.
#[cfg(any())]
pub fn cast_closure_to_fn_ptr() {
    (|b: u8| b as i8) as fn(u8) -> i8;
}

/// [`PointerCast::MutToConstPointer`]
/// 
/// ```shell
/// thread 'rustc' panicked at 'not yet implemented', c2rust-analyze/src/labeled_ty.rs:372:17
/// ```
#[cfg(any())]
pub fn cast_mut_to_const_ptr(p: *mut i32) {
    p as *const i32;
}

/// Meant to be [`PointerCast::ArrayToPointer`], but is [`CastKind::Misc`].
pub fn cast_array_ptr_to_ptr(p: *const [i32; 1]) {
    p as *const i32;
}

/// [`PointerCast::Unsize`]
///
/// ```shell
/// thread 'rustc' panicked at 'expected to find only one Assign statement, but got multiple', c2rust-analyze/src/rewrite/expr/hir_op.rs:126:21
/// ```
#[cfg(any())]
pub fn cast_unsize_direct(a: &[i32; 1]) {
    a as &[i32];
}

/// [`PointerCast::Unsize`]
pub fn cast_unsize_indirect(a: &[i32; 1]) {
    let _ = a.as_ptr();
}
