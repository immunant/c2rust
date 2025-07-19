use crate::lvalues::rust_lvalue;
use std::ffi::c_int;

#[link(name = "test")]
extern "C" {
    fn lvalue(_: *mut c_int);
}

const BUFFER_SIZE: usize = 6;

#[test]
pub fn test_lvalue() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [8, 9, 3, 6, 7, -8];

    unsafe {
        lvalue(buffer.as_mut_ptr());
        rust_lvalue(rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
