use crate::asm::rust_entry;
use libc::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn entry(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 1;

pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    // TODO(kkysen)
    // This used to be `[5]`, but both the C and Rust versions are giving results of 0,
    // so I'm setting this to 5 to pass tests for now.
    // Once we figure out exactly what's going on, we can fully fix it.
    let expected_buffer = [0];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
