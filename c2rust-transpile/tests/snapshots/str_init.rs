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
pub unsafe extern "C" fn ptr() {
    let mut _s: *const std::ffi::c_char = b"hello\0" as *const u8
        as *const std::ffi::c_char;
}
#[no_mangle]
pub unsafe extern "C" fn array_deduced_length() {
    let mut _s: [std::ffi::c_char; 6] = *::core::mem::transmute::<
        &[u8; 6],
        &mut [std::ffi::c_char; 6],
    >(b"hello\0");
}
#[no_mangle]
pub unsafe extern "C" fn array() {
    let mut _s: [std::ffi::c_char; 10] = *::core::mem::transmute::<
        &[u8; 10],
        &mut [std::ffi::c_char; 10],
    >(b"hello\0\0\0\0\0");
}
#[no_mangle]
pub unsafe extern "C" fn int_array_extra_braces() {
    let mut _a: [[std::ffi::c_int; 10]; 3] = [
        [
            1 as std::ffi::c_int,
            2 as std::ffi::c_int,
            3 as std::ffi::c_int,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
        ],
        [0; 10],
        [0; 10],
    ];
}
#[no_mangle]
pub unsafe extern "C" fn ptr_extra_braces() {
    let mut _s: *const std::ffi::c_char = b"hello\0" as *const u8
        as *const std::ffi::c_char;
}
#[no_mangle]
pub unsafe extern "C" fn array_extra_braces() {
    let _s: [std::ffi::c_char; 10] = *::core::mem::transmute::<
        &[u8; 10],
        &[std::ffi::c_char; 10],
    >(b"hello\0\0\0\0\0");
}
#[no_mangle]
pub unsafe extern "C" fn array_of_ptrs() {
    let mut _s: [*const std::ffi::c_char; 3] = [
        b"hello\0" as *const u8 as *const std::ffi::c_char,
        0 as *const std::ffi::c_char,
        0 as *const std::ffi::c_char,
    ];
}
