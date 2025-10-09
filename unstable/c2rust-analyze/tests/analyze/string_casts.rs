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

pub fn cast_array_to_ptr(s: &[u8; 1]) {
    s as *const u8;
}

pub fn cast_array_to_ptr_explicit(s: &[u8; 1]) {
    std::ptr::addr_of!(*s) as *const u8;
}

pub fn cast_from_literal() {
    b"\0" as *const u8 as *const core::ffi::c_char;
}

pub fn cast_from_literal_explicit() {
    std::ptr::addr_of!(*b"\0") as *const u8 as *const core::ffi::c_char;
}
