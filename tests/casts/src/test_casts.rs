use crate::cast_funptr::{rust_entry, rust_get_identity, rust_identity};
use crate::casts::rust_cast_stuff;

use std::ffi::{c_int, c_uint, c_void};

use std::mem::transmute;

#[link(name = "test")]
extern "C" {
    fn cast_stuff();

    fn identity(_: c_int) -> c_int;

    fn get_identity() -> *mut c_void;

    fn entry(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 2;

#[test]
pub fn test_compiles() {
    unsafe {
        cast_stuff();
        rust_cast_stuff();
    }
}

#[test]
pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    // 2nd element is a fn ptr address, which won't be the same.
    assert_eq!(buffer[..1], rust_buffer[..1]);
}

#[test]
pub fn test_identity() {
    for i in 0..10 {
        let id = unsafe { identity(i) };
        let rust_id = unsafe { rust_identity(i) };

        assert_eq!(id, i);
        assert_eq!(rust_id, i);
    }

    let transmuted_rust_identity: unsafe extern "C" fn(_: c_int) -> c_int =
        unsafe { transmute(rust_get_identity()) };
    let transmuted_identity: unsafe extern "C" fn(_: c_int) -> c_int =
        unsafe { transmute(get_identity()) };

    for i in 0..10 {
        let id = unsafe { transmuted_identity(i) };
        let rust_id = unsafe { transmuted_rust_identity(i) };

        assert_eq!(id, i);
        assert_eq!(rust_id, i);
    }
}
