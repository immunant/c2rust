pub fn cast_only(s: *const u8) {
    s as *const core::ffi::c_char;
}

pub fn deep_cast(x: *const *const u8) {
    x as *const *const i8;
}

#[cfg(any())]
pub fn cast_from_literal() {
    b"" as *const u8 as *const core::ffi::c_char;
}
