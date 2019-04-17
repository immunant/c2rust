extern crate libc;

use flex_array_members::{rust_exercise_flex_arrays};
use self::libc::{c_int, c_uint, size_t};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn exercise_flex_arrays(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 12;

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
