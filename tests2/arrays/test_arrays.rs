extern crate libc;

use arrays::entry as rust_entry;
use self::libc::{c_int, c_uint};

extern "C" {
    #[no_mangle]
    fn entry(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 34;

pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [
        97, 98, 99, 0, 100, 101, 102, 1, 
        0, 97, 98, 99, 0, 97, 98, 99, 100, 
        97, 98, 99, 97, 98, 99, 0, 0, 0, 
        0, 120, 0, 120, 0, 0, 120, 0
    ];

    unsafe {
       entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
       rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    for index in 0..BUFFER_SIZE {
        assert_eq!(buffer[index], rust_buffer[index]);
        assert_eq!(buffer[index], expected_buffer[index], "idx: {}", index);
    }
}
