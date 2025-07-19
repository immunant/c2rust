use crate::exprs::rust_exprs;
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn exprs(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 60;

#[test]
pub fn test_exprs() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];

    unsafe {
        exprs(BUFFER_SIZE as c_uint, buffer.as_mut_ptr());
        rust_exprs(BUFFER_SIZE as c_uint, rust_buffer.as_mut_ptr());
    }

    for x in 0..BUFFER_SIZE {
        assert_eq!(buffer[x], rust_buffer[x], "index {}", x);
    }
}
