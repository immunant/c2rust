extern crate libc;

use goto_linear_cf::{goto_linear as rust_goto_linear};
use goto_loop_cf::{goto_loop as rust_goto_loop};
use goto_switch_cf::{goto_switch as rust_goto_switch};

use self::libc::{c_int, c_uint};

extern "C" {
    #[no_mangle]
    fn goto_linear(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn goto_loop(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn goto_switch(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 4;
const BUFFER_SIZE2: usize = 12;
const BUFFER_SIZE3: usize = 6;

pub fn test_goto_linear() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [0, 1, 3, 2,];

    unsafe {
        goto_linear(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_goto_linear(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }
    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_goto_loop() {
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let expected_buffer = [0, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,];

    unsafe {
        goto_loop(BUFFER_SIZE2 as u32, buffer.as_mut_ptr());
        rust_goto_loop(BUFFER_SIZE2 as u32, rust_buffer.as_mut_ptr());
    }
    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_goto_switch() {
    let mut buffer = [0; BUFFER_SIZE3];
    let mut rust_buffer = [0; BUFFER_SIZE3];
    let expected_buffer = [0, 1, 1, 1, 2, 3,];

    unsafe {
        goto_switch(BUFFER_SIZE3 as u32, buffer.as_mut_ptr());
        rust_goto_switch(BUFFER_SIZE3 as u32, rust_buffer.as_mut_ptr());
    }
    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
