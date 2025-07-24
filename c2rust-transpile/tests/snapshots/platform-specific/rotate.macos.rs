#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]
#[no_mangle]
pub unsafe extern "C" fn rotate_left_64(
    mut x: std::ffi::c_ulonglong,
) -> std::ffi::c_ulonglong {
    return x.rotate_left(4 as std::ffi::c_int as std::ffi::c_ulonglong as u32);
}
#[no_mangle]
pub unsafe extern "C" fn rotate_right_64(
    mut x: std::ffi::c_ulonglong,
) -> std::ffi::c_ulonglong {
    return x.rotate_right(4 as std::ffi::c_int as std::ffi::c_ulonglong as u32);
}

