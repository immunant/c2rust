extern crate libc;

use forward::rust_forward;
use self::libc::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn forward(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 1;

pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [1];

    unsafe {
        forward(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_forward(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
