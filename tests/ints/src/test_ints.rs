use crate::chars::rust_multibyte_chars;
use crate::size_t::rust_entry;
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn entry(_: c_uint, _: *mut c_int);

    fn multibyte_chars(_: c_uint, _: *mut c_int) -> c_int;
}

const BUFFER_SIZE: usize = 10;

#[test]
pub fn test_size_t_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [0, 0, 0, 0, 0, 8, 0, 0, 0, 0];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

#[test]
pub fn test_chars_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = ['✓' as i32, '😱' as i32, '😱' as i32, 0, 1, -1, 0, 0, 0, 0];

    unsafe {
        assert!(multibyte_chars(BUFFER_SIZE as u32, buffer.as_mut_ptr()) as usize <= BUFFER_SIZE);
        assert!(
            rust_multibyte_chars(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr()) as usize
                <= BUFFER_SIZE
        );
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
