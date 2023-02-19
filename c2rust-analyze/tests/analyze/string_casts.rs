#[cfg(any())]
pub fn cast_only(s: *const u8) {
    s as *const core::ffi::c_char;
}

#[cfg(any())]
pub fn cast_from_literal() {
    b"" as *const u8 as *const core::ffi::c_char;
}
