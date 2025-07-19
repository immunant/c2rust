//! feature_c_variadic,

use crate::varargs::rust_call_printf;
// See #1281. Varargs don't yet work on aarch64.
#[cfg(not(target_arch = "aarch64"))]
use crate::varargs::{
    rust_call_vprintf, rust_my_printf, rust_restart_valist, rust_sample_stddev, rust_simple_vacopy,
    rust_valist_struct_member, rust_valist_struct_pointer_member,
};

use std::ffi::c_char;
use std::ffi::CString;

#[link(name = "test")]
extern "C" {
    fn call_printf();
}

// See #1281. Varargs don't yet work on aarch64.
#[cfg(not(target_arch = "aarch64"))]
#[link(name = "test")]
extern "C" {
    fn call_vprintf(_: *const c_char, ...);

    fn my_printf(_: *const c_char, ...);

    fn simple_vacopy(_: *const c_char, ...);

    fn valist_struct_member(_: *const c_char, ...);

    fn valist_struct_pointer_member(_: *const c_char, ...);

    fn restart_valist(_: *const c_char, ...);

    fn sample_stddev(count: i32, ...) -> f64;
}

// This test ensures we are able to define and call vararg prototypes
// that get linked in (IE printf)
#[test]
pub fn test_call_printf() {
    unsafe {
        call_printf();
        rust_call_printf();
    }
}

// Make sure we can pass through va_list arguments
#[cfg(not(target_arch = "aarch64"))]
#[test]
pub fn test_call_vprintf() {
    let fmt_str = CString::new("%d, %f\n").unwrap();
    unsafe {
        call_vprintf(fmt_str.as_ptr(), 10, 1.5);
        rust_call_vprintf(fmt_str.as_ptr(), 10, 1.5);
    }
}

// Test out a small varargs function definition
#[cfg(not(target_arch = "aarch64"))]
#[test]
pub fn test_my_printf() {
    let fmt_str = CString::new("%d, %f, %s\n").unwrap();
    let test_str = CString::new("test").unwrap();
    unsafe {
        my_printf(fmt_str.as_ptr(), 10, 1.5, test_str.as_ptr());
        rust_my_printf(fmt_str.as_ptr(), 10, 1.5, test_str.as_ptr());
    }
}

#[cfg(not(target_arch = "aarch64"))]
#[test]
pub fn test_simple_vacopy() {
    let fmt_str = CString::new("%d, %f\n").unwrap();
    unsafe {
        simple_vacopy(fmt_str.as_ptr(), 10, 1.5);
        rust_simple_vacopy(fmt_str.as_ptr(), 10, 1.5);
    }
}

#[cfg(not(target_arch = "aarch64"))]
#[test]
pub fn test_valist_struct_member() {
    let fmt_str = CString::new("%d, %f\n").unwrap();
    unsafe {
        valist_struct_member(fmt_str.as_ptr(), 10, 1.5);
        rust_valist_struct_member(fmt_str.as_ptr(), 10, 1.5);
    }
}

#[cfg(not(target_arch = "aarch64"))]
#[test]
pub fn test_valist_struct_pointer_member() {
    let fmt_str = CString::new("%d, %f\n").unwrap();
    unsafe {
        valist_struct_pointer_member(fmt_str.as_ptr(), 10, 1.5);
        rust_valist_struct_pointer_member(fmt_str.as_ptr(), 10, 1.5);
    }
}

#[cfg(not(target_arch = "aarch64"))]
#[test]
pub fn test_restart_valist() {
    let fmt_str = CString::new("%d, %f\n").unwrap();
    unsafe {
        restart_valist(fmt_str.as_ptr(), 10, 1.5);
        rust_restart_valist(fmt_str.as_ptr(), 10, 1.5);
    }
}

#[cfg(not(target_arch = "aarch64"))]
#[test]
pub fn test_sample_stddev() {
    unsafe {
        let c_res = sample_stddev(4, 25.0, 27.3, 26.9, 25.7);
        let rs_res = rust_sample_stddev(4, 25.0, 27.3, 26.9, 25.7);
        assert_eq!(c_res, rs_res);
    }
}
