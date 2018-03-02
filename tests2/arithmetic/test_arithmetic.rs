extern crate libc;

use arithmetic::entry as rust_entry;
use self::libc::{c_uint, c_int};

extern "C" {
    #[no_mangle]
    fn entry(_: c_uint, _: *mut c_int);
}

pub fn test_buffer() {
    let mut buffer = [0; 100];
    let mut rust_buffer = [0; 100];
    let expected_buffer = [
        1, 2, 0, 1, 1, 32, -2, 255, 8, 14,
        19660800, 18, 151, 2, 1, 0, 0, 0, 1, 1,
        1, 1, 15, 0, 1, 0, 1, 0, 1, 1,
        0, 0, 0, 0, 1, 1, 1, 0, 0, 1,
        1, 10, -10, 900, 11, 1, 9, 1, 14, 80,
        125, 99, 98, -1001, 0, 1, -1000, 1000, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];

    unsafe {
        entry(100, buffer.as_mut_ptr());
        rust_entry(100, rust_buffer.as_mut_ptr());
    }

    for index in 0..100 {
        assert_eq!(buffer[index], rust_buffer[index]);
        assert_eq!(buffer[index], expected_buffer[index]);
    }
}
