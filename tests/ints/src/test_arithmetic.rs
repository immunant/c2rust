use crate::arithmetic::rust_entry2;
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn entry2(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 100;

#[test]
pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [
        1, 2, 0, 1, 1, 32, -2, 255, 8, 14, 19660800, 18, 151, 2, 1, 0, 0, 0, 1, 1, 1, 1, 15, 0, 1,
        0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 10, -10, 900, 11, 1, 9, 1, 14, 80, 125, 99,
        98, -1001, 0, 1, -1000, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];

    unsafe {
        entry2(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry2(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    for index in 0..BUFFER_SIZE {
        assert_eq!(buffer[index], rust_buffer[index]);
        assert_eq!(buffer[index], expected_buffer[index]);
    }
}
