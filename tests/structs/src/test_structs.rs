extern crate libc;

use std::mem::align_of;
use structs::{Aligned8Struct, rust_entry};
use self::libc::{c_int, c_uint, size_t};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn entry(_: c_uint, _: *mut c_int);
    #[no_mangle]
    fn alignment_of_aligned8_struct() -> size_t;
}

const BUFFER_SIZE: usize = 9;

pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [1, 2, 3, 10, 20, 0, 0, 0, 0];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_alignment() {
    let c_alignment = unsafe {
        alignment_of_aligned8_struct()
    };

    assert_eq!(align_of::<Aligned8Struct>(), c_alignment);
}
