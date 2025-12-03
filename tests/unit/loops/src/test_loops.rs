use crate::break_continue::rust_entry;
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn entry(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 70;

#[test]
pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [
        1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4,
        5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 6, 0, 6, 0, 6, 0, 6, 0, 0, 0, 0, 0, 7, 0, 7, 0, 7,
        0, 7, 0, 7, 0, 7, 0, 0, 0, 0,
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
