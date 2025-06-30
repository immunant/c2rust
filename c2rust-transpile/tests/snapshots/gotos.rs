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
pub unsafe extern "C" fn sum(mut count: std::ffi::c_int) -> std::ffi::c_int {
    let mut x: std::ffi::c_int = 0 as std::ffi::c_int;
    while !(count <= 0 as std::ffi::c_int) {
        x += count;
        count -= 1;
        count;
    }
    return x;
}
