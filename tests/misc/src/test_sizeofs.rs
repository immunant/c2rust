//! feature_core_intrinsics,
extern crate libc;

use sizeofs::rust_sizeofs;
use self::libc::c_int;
use self::libc::c_uint;

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn sizeofs(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 60;

pub fn test_sizeofs() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];

    unsafe {
        sizeofs(BUFFER_SIZE as c_uint, buffer.as_mut_ptr());
        rust_sizeofs(BUFFER_SIZE as c_uint, rust_buffer.as_mut_ptr());
    }

    for x in 0..BUFFER_SIZE {
        assert_eq!(buffer[x], rust_buffer[x], "index {}", x);
    }
}
