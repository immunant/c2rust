
pub unsafe fn json_c_version() -> *const u8 {
    (*b"0.12.1\0").as_ptr()
}
#[export_name = "json_c_version"]
pub unsafe extern "C" fn json_c_version_wrapper() -> *const u8 {
    json_c_version()
}

pub unsafe fn json_c_version_num() -> i32 {
    0i32 << 16i32 | 12i32 << 8i32 | 1i32
}
#[export_name = "json_c_version_num"]
pub unsafe extern "C" fn json_c_version_num_wrapper() -> i32 {
    json_c_version_num()
}
