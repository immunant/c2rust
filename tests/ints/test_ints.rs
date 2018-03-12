extern crate libc;

use size_t::entry as rust_entry;
use self::libc::{c_int, c_uint};

extern "C" {
    #[no_mangle]
    fn entry(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 10;

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
