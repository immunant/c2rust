use crate::struct_with_exp::rust_struct_with_exp;
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn struct_with_exp(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 1;

#[test]
pub fn test_struct_with_exp() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [42];

    unsafe {
        struct_with_exp(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_struct_with_exp(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
