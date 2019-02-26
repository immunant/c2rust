//! feature_core_intrinsics, extern_crate_core
extern crate libc;

use atomics::rust_atomics_entry;
use self::libc::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn atomics_entry(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 1024;

pub fn test_atomics() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];

    unsafe {
       atomics_entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
       rust_atomics_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    for index in 0..BUFFER_SIZE {
        assert_eq!(buffer[index], rust_buffer[index]);
    }
}
