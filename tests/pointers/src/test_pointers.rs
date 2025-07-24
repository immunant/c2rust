//! feature_c_variadic

use crate::function_pointers::rust_entry3;
use crate::pointer_arith::rust_entry2;
use crate::pointer_init::rust_entry;
use crate::ref_decay::{
    rust_address_cast, rust_bar, rust_bitcast, rust_calls_all, rust_f, rust_foobar,
};
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn entry(_: c_uint, _: *mut c_int);

    fn entry2(_: c_uint, _: *mut c_int);

    fn entry3(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 5;
const BUFFER_SIZE2: usize = 31;
const BUFFER_SIZE3: usize = 18;

#[test]
pub fn test_init() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [115, 116, 114, 105, 110];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

#[test]
pub fn test_arith() {
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let expected_buffer = [
        1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 33, 1, 1, 1, 1, 1, 1, 35, 1, 1,
        34,
    ];

    unsafe {
        entry2(BUFFER_SIZE2 as u32, buffer.as_mut_ptr());
        rust_entry2(BUFFER_SIZE2 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

#[test]
pub fn test_fn_ptrs() {
    let mut buffer = [0; BUFFER_SIZE3];
    let mut rust_buffer = [0; BUFFER_SIZE3];
    let expected_buffer = [
        97, 97, 97, -98, 1, 0, 0, 1, 65, 66, 68, 69, 97, 97, 97, 1, 97, 98,
    ];
    #[cfg(target_arch = "aarch64")]
    let expected_buffer = [
        97, 97, 97, -98, 1, 0, 0, 1, 68, 69, 97, 97, 97, 1, 0, 0, 0, 0,
    ];

    unsafe {
        entry3(BUFFER_SIZE3 as u32, buffer.as_mut_ptr());
        rust_entry3(BUFFER_SIZE3 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(&buffer[..], &expected_buffer[..], "c version");
    assert_eq!(&rust_buffer[..], &expected_buffer[..], "rust version");
}
