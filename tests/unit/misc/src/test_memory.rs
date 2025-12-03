use crate::malloc::rust_malloc_test;
use crate::strings_h::rust_setmem;
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn malloc_test(_: c_uint, _: *mut c_int);

    fn setmem(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 3;
const BUFFER_SIZE2: usize = 5;

#[test]
pub fn test_malloc() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [34, 35, 36];

    unsafe {
        malloc_test(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_malloc_test(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

#[test]
pub fn test_memset() {
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let expected_buffer = [16843009; BUFFER_SIZE2];

    unsafe {
        setmem(BUFFER_SIZE2 as u32, buffer.as_mut_ptr());
        rust_setmem(BUFFER_SIZE2 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
