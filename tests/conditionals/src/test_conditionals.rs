use crate::binary_conditional::rust_entry3;
use crate::conditional::rust_entry;
use crate::conditionals::{rust_entry2, rust_ternaries};
use crate::else_if_chain::rust_entry4;
use crate::unused_conditionals::{
    rust_unused_conditional1, rust_unused_conditional2, rust_unused_conditional3,
};
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn entry(_: c_uint, _: *mut c_int);

    fn entry2(_: c_uint, _: *mut c_int);

    fn entry3(_: c_uint, _: *mut c_int);

    fn entry4(_: c_int) -> c_int;

    fn unused_conditional1() -> c_int;
    fn unused_conditional2() -> c_int;
    fn unused_conditional3() -> c_int;
}

const BUFFER_SIZE: usize = 4;
const BUFFER_SIZE2: usize = 30;
const BUFFER_SIZE3: usize = 6;

#[test]
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

#[test]
pub fn test_buffer2() {
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let expected_buffer = [
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];

    unsafe {
        entry2(BUFFER_SIZE2 as u32, buffer.as_mut_ptr());
        rust_entry2(BUFFER_SIZE2 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

#[test]
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

#[test]
pub fn test_unused_conditional() {
    unsafe {
        assert_eq!(unused_conditional1(), rust_unused_conditional1());
        assert_eq!(unused_conditional2(), rust_unused_conditional2());
        assert_eq!(unused_conditional3(), rust_unused_conditional3());
    }
}

#[test]
pub fn test_else_if_chain() {
    unsafe {
        assert_eq!(entry4(0), rust_entry4(0));
        assert_eq!(entry4(10), rust_entry4(10));
        assert_eq!(entry4(20), rust_entry4(20));
        assert_eq!(entry4(30), rust_entry4(30));
    }
}
