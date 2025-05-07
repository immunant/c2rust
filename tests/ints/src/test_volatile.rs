use crate::volatile::{rust_entry3, rust_volatile_stuff};
use libc::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn volatile_stuff();

    fn entry3(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 9;

pub fn test_compiles() {
    unsafe {
        volatile_stuff();
        rust_volatile_stuff();
    }
}

pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [5, 11, 5, 9, 99, 116, 101, 115, 116];

    unsafe {
        entry3(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry3(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
