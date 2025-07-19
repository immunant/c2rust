use crate::shadowing::{rust_shadow, rust_twice};
use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn twice(_: c_int) -> c_int;

    fn shadow(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 10;

#[test]
pub fn test_twice() {
    for i in 0..20 {
        let double = unsafe { twice(i) };
        let rust_double = unsafe { rust_twice(i) };

        assert_eq!(double, rust_double);
    }
}

#[test]
pub fn test_shadowing() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [10, 6, 12, 18, 24, 30, 36, 42, 48, 54];

    unsafe {
        shadow(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_shadow(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
