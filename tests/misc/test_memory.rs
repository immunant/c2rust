extern crate libc;

use strings_h::setmem as rust_setmem;
use malloc::{malloc_test as rust_malloc_test};
use self::libc::{c_int, c_uint};

extern "C" {
    #[no_mangle]
    fn malloc_test(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn setmem(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 3;
const BUFFER_SIZE2: usize = 5;

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

pub fn test_memset() {
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let expected_buffer = [16843009; BUFFER_SIZE2];

    unsafe {
        setmem(BUFFER_SIZE2 as u32, buffer.as_mut_ptr());
        rust_setmem(BUFFER_SIZE2 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
