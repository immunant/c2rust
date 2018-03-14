extern crate libc;

use const_test::rust_entry4;
use self::libc::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn entry4(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 2;

pub fn test_const() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [3, 2];

    unsafe {
        entry4(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry4(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
