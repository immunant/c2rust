extern crate libc;

use malloc::{malloc_test as rust_malloc_test};
use self::libc::{c_int, c_uint};

extern "C" {
    #[no_mangle]
    fn malloc_test(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 3;

pub fn test_malloc() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [34, 35, 36];

    unsafe {
        malloc_test(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_malloc_test(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
