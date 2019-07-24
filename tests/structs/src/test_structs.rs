extern crate libc;

use std::mem::align_of;
use structs::{Aligned8Struct, rust_entry, rust_alignment_entry};
use self::libc::{c_int, c_uint, size_t};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn entry(_: c_uint, _: *mut c_int);
    #[no_mangle]
    fn alignment_of_aligned8_struct() -> size_t;
    #[no_mangle]
    fn alignment_entry(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 9;
const ALIGNMENT_BUFFER_SIZE: usize = 176;

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

pub fn test_alignments() {
    let mut buffer = [0; ALIGNMENT_BUFFER_SIZE];
    let mut rust_buffer = [0; ALIGNMENT_BUFFER_SIZE];
    let expected_buffer = [
        16,  8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S1
        11,  1, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S2
        12,  2, 42, 1337, 65537, 0, 2, 4, 0, 0, 0, // S3
        12,  4, 42, 1337, 65537, 0, 2, 4, 0, 0, 0, // S4
        16,  8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S5
        11,  1, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S6
        16,  8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S7
        16,  8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S8
        16,  8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S9
        16,  8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S10
        16, 16, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S11
        11,  1, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S12
        12,  2, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S13
        12,  4, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S14
        16,  8, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S15
        16, 16, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S16
    ];

    unsafe {
        alignment_entry(ALIGNMENT_BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_alignment_entry(ALIGNMENT_BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(&buffer[..], &rust_buffer[..]);
    assert_eq!(&buffer[..], &expected_buffer[..]);
}
