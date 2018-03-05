extern crate libc;

use enum_as_int::{E, entry as rust_entry};
use enum_ret::{Color, entry2 as rust_entry2};
use self::libc::{c_int, c_uint};

extern "C" {
    #[no_mangle]
    fn entry(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn entry2(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 10;
const BUFFER_SIZE2: usize = 7;

pub fn test_variants() {
    assert_eq!(E::A as u32, 0);
    assert_eq!(E::B as u32, 1);
}

pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [1, 1, 1, 1, 1, 1, 0, 0, 0, 0];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_buffer2() {
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let expected_buffer = [1, 2, -1, 1, -2, 1, 6];

    unsafe {
        entry2(BUFFER_SIZE2 as u32, buffer.as_mut_ptr());
        rust_entry2(BUFFER_SIZE2 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
