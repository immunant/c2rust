//! feature_core_intrinsics, extern_crate_core
extern crate libc;

use atomics::{rust_atomics_entry, rust_new_atomics};
use mem_x_fns::rust_mem_x;
use math::{rust_ffs, rust_ffsl, rust_ffsll};
use self::libc::{c_int, c_uint, c_char, c_long, c_longlong};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn atomics_entry(_: c_uint, _: *mut c_int);
    #[no_mangle]
    fn new_atomics(_: c_uint, _: *mut c_int);
    #[no_mangle]
    fn mem_x(_: *const c_char, _: *mut c_char);
    #[no_mangle]
    fn ffs(_: c_int) -> c_int;
    #[no_mangle]
    fn ffsl(_: c_long) -> c_int;
    #[no_mangle]
    fn ffsll(_: c_longlong) -> c_int;
}

const BUFFER_SIZE: usize = 1024;
const BUFFER_SIZE2: usize = 10;

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

pub fn test_ffs() {
    for i in 0..256 {
        let ffs_ret = unsafe {
            ffs(i)
        };
        let rust_ffs_ret = unsafe {
            rust_ffs(i)
        };

        assert_eq!(ffs_ret, rust_ffs_ret);

        let ffsl_ret = unsafe {
            ffsl(i as i64)
        };
        let rust_ffsl_ret = unsafe {
            rust_ffsl(i as i64)
        };

        assert_eq!(ffsl_ret, rust_ffsl_ret);

        let ffsll_ret = unsafe {
            ffsll(i as i64)
        };
        let rust_ffsll_ret = unsafe {
            rust_ffsll(i as i64)
        };

        assert_eq!(ffsll_ret, rust_ffsll_ret);
    }
}
