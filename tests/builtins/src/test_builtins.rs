//! feature_core_intrinsics, extern_crate_core
extern crate libc;

use atomics::rust_atomics_entry;
use mem_x_fns::rust_mem_x;
use self::libc::{c_int, c_uint, c_char};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn atomics_entry(_: c_uint, _: *mut c_int);
    #[no_mangle]
    fn mem_x(_: *const c_char, _: *mut c_char);
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
