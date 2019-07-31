//! feature_c_variadic,
extern crate libc;

use varargs::{rust_call_printf, rust_call_vprintf, rust_my_printf, rust_simple_vacopy,
              rust_restart_valist, rust_sample_stddev};

use std::ffi::CString;
use self::libc::c_char;

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn call_printf();

    #[no_mangle]
    fn call_vprintf(_: *const c_char, ...);

    #[no_mangle]
    fn my_printf(_: *const c_char, ...);

    #[no_mangle]
    fn simple_vacopy(_: *const c_char, ...);

    #[no_mangle]
    fn restart_valist(_: *const c_char, ...);

    #[no_mangle]
    fn sample_stddev(count: i32, ...) -> f64;
}

// This test ensures we are able to define and call vararg prototypes
// that get linked in (IE printf)
pub fn test_call_printf() {
    unsafe {
        call_printf();
        rust_call_printf();
    }
}

// Make sure we can pass through va_list arguments
pub fn test_call_vprintf() {
    let fmt_str = CString::new("%d, %f\n").unwrap();
    unsafe {
        call_vprintf(fmt_str.as_ptr(), 10, 1.5);
        rust_call_vprintf(fmt_str.as_ptr(), 10, 1.5);
    }
}

// Test out a small varargs function definition
pub fn test_my_printf() {
    let fmt_str = CString::new("%d, %f, %s\n").unwrap();
    let test_str = CString::new("test").unwrap();
    unsafe {
        my_printf(fmt_str.as_ptr(), 10, 1.5, test_str.as_ptr());
        rust_my_printf(fmt_str.as_ptr(), 10, 1.5, test_str.as_ptr());
    }
}

pub fn test_simple_vacopy() {
     let fmt_str = CString::new("%d, %f\n").unwrap();
     unsafe {
         simple_vacopy(fmt_str.as_ptr(), 10, 1.5);
         rust_simple_vacopy(fmt_str.as_ptr(), 10, 1.5);
     }
 }

pub fn test_restart_valist() {
    let fmt_str = CString::new("%d, %f\n").unwrap();
    unsafe {
        restart_valist(fmt_str.as_ptr(), 10, 1.5);
        rust_restart_valist(fmt_str.as_ptr(), 10, 1.5);
    }
}

pub fn test_sample_stddev() {
    unsafe {
        let c_res= sample_stddev(4, 25.0, 27.3, 26.9, 25.7);
        let rs_res= rust_sample_stddev(4, 25.0, 27.3, 26.9, 25.7);
        assert_eq!(c_res, rs_res);
    }
}