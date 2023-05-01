pub fn cast_ptr_to_ptr(s: *const u8) {
    s as *const core::ffi::c_char;
}

pub fn deep_cast_ptr_to_ptr(x: *const *const u8) {
    x as *const *const i8;
}

pub fn cast_slice_ptr_to_ptr(s: *const [u8]) {
    s as *const u8;
}

pub fn cast_array_to_slice_ptr(s: &[u8; 0]) {
    s as *const [u8];
}

pub fn cast_array_to_ptr(s: &[u8; 0]) {
    s as *const u8;
}

pub fn cast_from_literal() {
    b"" as *const u8 as *const core::ffi::c_char;
}
