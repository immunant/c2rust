
#[no_mangle]
pub unsafe extern fn json_c_version() -> *const u8 {
    (*b"0.12.1\0").as_ptr()
}

#[no_mangle]
pub unsafe extern fn json_c_version_num() -> i32 {
    0i32 << 16i32 | 12i32 << 8i32 | 1i32
}
