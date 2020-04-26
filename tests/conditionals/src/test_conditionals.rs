extern crate libc;

use conditional::rust_entry;
use conditionals::{rust_entry2, rust_ternaries};
use binary_conditional::rust_entry3;
use unused_conditionals::{rust_unused_conditional1, rust_unused_conditional2, rust_unused_conditional3};
use self::libc::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn entry(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn entry2(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn entry3(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn unused_conditional1() -> c_int;
    #[no_mangle]
    fn unused_conditional2() -> c_int;
    #[no_mangle]
    fn unused_conditional3() -> c_int;
}

const BUFFER_SIZE: usize = 4;
const BUFFER_SIZE2: usize = 30;
const BUFFER_SIZE3: usize = 6;

pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [0, 0, 2, 3];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_buffer2() {
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let expected_buffer = [
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];

    unsafe {
        entry2(BUFFER_SIZE2 as u32, buffer.as_mut_ptr());
        rust_entry2(BUFFER_SIZE2 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_binary_conditionals() {
    let mut buffer = [0; BUFFER_SIZE3];
    let mut rust_buffer = [0; BUFFER_SIZE3];
    let expected_buffer = [1, 2, 2, 3, 4, 0];

    unsafe {
        entry3(BUFFER_SIZE3 as u32, buffer.as_mut_ptr());
        rust_entry3(BUFFER_SIZE3 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_unused_conditional() {
  unsafe {
    assert_eq!(unused_conditional1(), rust_unused_conditional1());
    assert_eq!(unused_conditional2(), rust_unused_conditional2());
    assert_eq!(unused_conditional3(), rust_unused_conditional3());
  }
}
