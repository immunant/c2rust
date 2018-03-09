extern crate libc;

use compound_assignment::compound_assignment as rust_compound_assignment;

use self::libc::{c_int, c_uint};

extern "C" {
    #[no_mangle]
    fn compound_assignment(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 13;

pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [129, 0, 55, 0, 0, 0, 0, 55, 0, 0, 2100, 129, 183];

    unsafe {
        compound_assignment(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_compound_assignment(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
