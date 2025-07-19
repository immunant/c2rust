use crate::arrays::rust_entry;
use crate::incomplete_arrays::{rust_check_some_ints, rust_entry2, rust_test_sized_array};
use crate::variable_arrays::{rust_alloca_arrays, rust_variable_arrays};
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn entry(_: c_uint, _: *mut c_int);

    fn entry2(_: c_uint, _: *mut c_int);

    fn test_sized_array() -> c_uint;

    fn variable_arrays(_: *mut c_int);

    fn alloca_arrays(_: *mut c_int);

    fn check_some_ints() -> bool;
}

#[no_mangle]
pub static SOME_INTS: [u32; 4] = [2, 0, 1, 8];
#[no_mangle]
pub static rust_SOME_INTS: [u32; 4] = [2, 0, 1, 8];

const BUFFER_SIZE: usize = 49;
const BUFFER_SIZE2: usize = 2;
const BUFFER_SIZEV: usize = 88;

#[test]
pub fn test_sized_array_impls() {
    unsafe {
        assert_eq!(rust_test_sized_array(), test_sized_array());
    }
}

#[test]
pub fn test_global_incomplete_array() {
    unsafe {
        assert_eq!(rust_check_some_ints(), check_some_ints());
    }
}

#[test]
pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [
        97, 98, 99, 0, 100, 101, 102, 1, 0, 97, 98, 99, 0, 97, 98, 99, 100, 97, 98, 99, 97, 98, 99,
        0, 0, 0, 0, 120, 0, 120, 0, 0, 120, 109, 121, 115, 116, 114, 105, 110, 103, 109, 121, 115,
        116, 114, 105, 110, 103,
    ];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    for index in 0..BUFFER_SIZE {
        assert_eq!(buffer[index], rust_buffer[index]);
        assert_eq!(buffer[index], expected_buffer[index], "index: {}", index);
    }
}

#[test]
pub fn test_buffer2() {
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let expected_buffer = [1, 1];

    unsafe {
        entry2(BUFFER_SIZE2 as u32, buffer.as_mut_ptr());
        rust_entry2(BUFFER_SIZE2 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

#[test]
pub fn test_variable_arrays() {
    let mut buffer = [0; BUFFER_SIZEV];
    let mut rust_buffer = [0; BUFFER_SIZEV];
    let expected_buffer = [
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
        26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
        11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
        34, 35, 36, 37, 38, 39, 40, 0, 3, 6, 9, 12, 15, 18, 21,
    ];
    unsafe {
        variable_arrays(buffer.as_mut_ptr());
        rust_variable_arrays(rust_buffer.as_mut_ptr());
    }

    for index in 0..BUFFER_SIZEV {
        assert_eq!(buffer[index], expected_buffer[index], "index: {}", index);
        assert_eq!(buffer[index], rust_buffer[index], "index: {}", index);
    }
}

#[test]
pub fn test_alloca_arrays() {
    let mut buffer = [0; BUFFER_SIZEV];
    let mut rust_buffer = [0; BUFFER_SIZEV];
    let expected_buffer = [
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
        26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
        11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
        34, 35, 36, 37, 38, 39, 40, 0, 3, 6, 9, 12, 15, 18, 21,
    ];
    unsafe {
        alloca_arrays(buffer.as_mut_ptr());
        rust_alloca_arrays(rust_buffer.as_mut_ptr());
    }

    for index in 0..BUFFER_SIZEV {
        assert_eq!(buffer[index], expected_buffer[index], "index: {}", index);
        assert_eq!(buffer[index], rust_buffer[index], "index: {}", index);
    }
}
