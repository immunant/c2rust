extern crate libc;

use arrays::entry as rust_entry;
use incomplete_arrays::entry2 as rust_entry2;
use self::libc::{c_int, c_uint};

extern "C" {
    #[no_mangle]
    fn entry(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn entry2(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 49;
const BUFFER_SIZE2: usize = 2;

pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [
        97, 98, 99, 0, 100, 101, 102, 1, 
        0, 97, 98, 99, 0, 97, 98, 99, 100, 
        97, 98, 99, 97, 98, 99, 0, 0, 0, 
        0, 120, 0, 120, 0, 0, 120, 109, 
        121, 115, 116, 114, 105, 110, 103, 
        109, 121, 115, 116, 114, 105, 110, 
        103,
    ];

    unsafe {
       entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
       rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    for index in 0..BUFFER_SIZE {
        assert_eq!(buffer[index], rust_buffer[index]);
        assert_eq!(buffer[index], expected_buffer[index], "index: {}", index);
    }
}

pub fn test_buffer2() {
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let expected_buffer = [1, 1];

    unsafe {
       entry2(BUFFER_SIZE2 as u32, buffer.as_mut_ptr());
       rust_entry2(BUFFER_SIZE2 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}