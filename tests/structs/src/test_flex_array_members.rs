use crate::flex_array_members::rust_exercise_flex_arrays;
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn exercise_flex_arrays(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 12;

#[test]
pub fn test_flex_array_members() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [0, 0, 0, 10, 0, 0, 0, 10, 0, 0, 0, 15];

    unsafe {
        exercise_flex_arrays(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_exercise_flex_arrays(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
