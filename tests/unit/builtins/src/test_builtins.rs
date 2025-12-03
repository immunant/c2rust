//! feature_core_intrinsics

use crate::alloca::rust_alloca_hello;
use crate::atomics::{rust_atomics_entry, rust_new_atomics};
use crate::math::{rust_ffs, rust_ffsl, rust_ffsll, rust_isfinite, rust_isinf_sign, rust_isnan};
use crate::mem_x_fns::{rust_assume_aligned, rust_mem_x};
use std::ffi::{c_char, c_double, c_int, c_long, c_longlong, c_uint};

#[link(name = "test")]
extern "C" {
    fn alloca_hello() -> c_int;
    fn atomics_entry(_: c_uint, _: *mut c_int);
    fn new_atomics(_: c_uint, _: *mut c_int);
    fn mem_x(_: *const c_char, _: *mut c_char);
    fn ffs(_: c_int) -> c_int;
    fn ffsl(_: c_long) -> c_int;
    fn ffsll(_: c_longlong) -> c_int;
    fn isfinite(_: c_double) -> c_int;
    fn isnan(_: c_double) -> c_int;
    fn isinf_sign(_: c_double) -> c_int;
}

const BUFFER_SIZE: usize = 1024;
const BUFFER_SIZE2: usize = 10;

#[test]
pub fn test_alloca() {
    unsafe {
        alloca_hello();
        rust_alloca_hello();
    }
}

#[test]
pub fn test_atomics() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];

    unsafe {
        atomics_entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_atomics_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    for index in 0..BUFFER_SIZE {
        assert_eq!(buffer[index], rust_buffer[index]);
    }
}

#[test]
pub fn test_new_atomics() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];

    unsafe {
        new_atomics(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_new_atomics(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    for index in 0..BUFFER_SIZE {
        eprintln!("buffer[{}] = {}", index, buffer[index]);
        assert_eq!(buffer[index], rust_buffer[index]);
    }
}

#[test]
pub fn test_mem_fns() {
    let const_string = "I am ten!\0";
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let const_str_ptr = const_string.as_ptr() as *const i8;

    unsafe {
        mem_x(const_str_ptr, buffer.as_mut_ptr());
        rust_mem_x(const_str_ptr, rust_buffer.as_mut_ptr());
    }

    for index in 0..BUFFER_SIZE2 {
        assert_eq!(buffer[index], rust_buffer[index]);
    }
}

#[test]
pub fn test_ffs() {
    for i in 0..256 {
        let ffs_ret = unsafe { ffs(i) };
        let rust_ffs_ret = unsafe { rust_ffs(i) };

        assert_eq!(ffs_ret, rust_ffs_ret);

        let ffsl_ret = unsafe { ffsl(i as i64) };
        let rust_ffsl_ret = unsafe { rust_ffsl(i as i64) };

        assert_eq!(ffsl_ret, rust_ffsl_ret);

        let ffsll_ret = unsafe { ffsll(i as i64) };
        let rust_ffsll_ret = unsafe { rust_ffsll(i as i64) };

        assert_eq!(ffsll_ret, rust_ffsll_ret);
    }
}

#[test]
pub fn test_clang9_intrinsics() {
    let pinf = 1.0 / 0.0;
    let ninf = -1.0 / 0.0;
    let fin = 1.0;

    // isfinite
    for i in &[pinf, fin] {
        let isfinite_ret = unsafe { isfinite(*i) };

        let rust_isfinite_ret = unsafe { rust_isfinite(*i) };

        assert_eq!(isfinite_ret, rust_isfinite_ret);
    }

    // isnan
    let nan = 0.0 / 0.0;
    let an = 1.0;

    for i in &[nan, an] {
        let isnan_ret = unsafe { isnan(*i) };

        let rust_isnan_ret = unsafe { rust_isnan(*i) };

        assert_eq!(isnan_ret, rust_isnan_ret);
    }

    // isinf_sign
    for i in &[pinf, ninf, fin] {
        let isinf_sign_ret = unsafe { isinf_sign(*i) };

        let rust_isinf_sign_ret = unsafe { rust_isinf_sign(*i) };

        assert_eq!(isinf_sign_ret, rust_isinf_sign_ret);
    }
}

#[test]
pub fn test_assume_aligned() {
    let null = std::ptr::null_mut();

    unsafe {
        assert_eq!(rust_assume_aligned(null), null);
    }
}
