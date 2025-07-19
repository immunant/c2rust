use crate::unions::rust_entry;
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn entry(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 19;

#[test]
pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [12, 12, 0, 5, 1, 2, 3, 4, 0, 5, 6, 7, 8, 0, 8, 9, 10, 12, 18];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
