extern crate libc;

use storage::{entry as rust_entry};
use self::libc::{c_int, c_uint};

extern "C" {
    #[no_mangle]
    fn entry(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 11;

// xfail
pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [1, 4, 2, 0, 0, 0, 0, 4, 4, 104, 111]; // FIXME

    // REVIEW: C and Rust seem to be producing different results when called side by side,
    // I suspect some sort of UB buffer overflow going on
    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer,expected_buffer);
}
