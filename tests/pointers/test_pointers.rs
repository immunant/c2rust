extern crate libc;

use pointer_init::rust_entry;
use pointer_arith::rust_entry2;
use function_pointers::rust_entry3;
use ref_decay::{rust_f, rust_bar, rust_bitcast, rust_foobar, rust_calls_all};
use self::libc::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn entry(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn entry2(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn entry3(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 5;
const BUFFER_SIZE2: usize = 31;
const BUFFER_SIZE3: usize = 8;

pub fn test_init() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [115, 116, 114, 105, 110];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_arith() {
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let expected_buffer = [
        1, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        33, 1, 1, 1, 1, 1, 1, 35, 1, 1,
        34,
    ];

    unsafe {
        entry2(BUFFER_SIZE2 as u32, buffer.as_mut_ptr());
        rust_entry2(BUFFER_SIZE2 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_fn_ptrs() {
    let mut buffer = [0; BUFFER_SIZE3];
    let mut rust_buffer = [0; BUFFER_SIZE3];
    let expected_buffer = [97, 97, 97, -98, 1, 0, 0, 1];

    unsafe {
        entry3(BUFFER_SIZE3 as u32, buffer.as_mut_ptr());
        rust_entry3(BUFFER_SIZE3 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(&buffer[..],      &expected_buffer[..], "c version");
    assert_eq!(&rust_buffer[..], &expected_buffer[..], "rust version");
}
