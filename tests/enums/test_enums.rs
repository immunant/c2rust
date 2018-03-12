extern crate libc;

use enum_as_int::{E, entry as rust_entry};
use enum_ret::{Color, entry2 as rust_entry2};
use enum_duplicate::{e, entry3 as rust_entry3};
use top_enum::{E as otherE, entry4 as rust_entry4};
use big_enum::{E1, E2, E3, entry5 as rust_entry5};

use self::libc::{c_int, c_uint};

extern "C" {
    #[no_mangle]
    fn entry(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn entry2(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn entry3(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn entry4(_: c_uint, _: *mut c_int);

    #[no_mangle]
    fn entry5(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 10;
const BUFFER_SIZE2: usize = 7;
const BUFFER_SIZE3: usize = 4;
const BUFFER_SIZE4: usize = 1;
const BUFFER_SIZE5: usize = 6;


pub fn test_variants() {
    assert_eq!(E::A as u32, 0);
    assert_eq!(E::B as u32, 1);

}

pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [1, 1, 1, 1, 1, 1, 0, 0, 0, 0];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_buffer2() {
    let mut buffer = [0; BUFFER_SIZE2];
    let mut rust_buffer = [0; BUFFER_SIZE2];
    let expected_buffer = [1, 2, -1, 1, -2, 1, 6];

    unsafe {
        entry2(BUFFER_SIZE2 as u32, buffer.as_mut_ptr());
        rust_entry2(BUFFER_SIZE2 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_buffer3() {
    let mut buffer = [0; BUFFER_SIZE3];
    let mut rust_buffer = [0; BUFFER_SIZE3];
    let expected_buffer = [0, 0, -10, -9];

    unsafe {
        entry3(BUFFER_SIZE3 as u32, buffer.as_mut_ptr());
        rust_entry3(BUFFER_SIZE3 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_buffer4() {
    let mut buffer = [0; BUFFER_SIZE4];
    let mut rust_buffer = [0; BUFFER_SIZE4];
    let expected_buffer = [1];

    unsafe {
        entry4(BUFFER_SIZE4 as u32, buffer.as_mut_ptr());
        rust_entry4(BUFFER_SIZE4 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_buffer5() {
    let mut buffer = [0; BUFFER_SIZE5];
    let mut rust_buffer = [0; BUFFER_SIZE5];
    let expected_buffer = [1, 0, 1, 0, 1, 0];

    unsafe {
        entry5(BUFFER_SIZE5 as u32, buffer.as_mut_ptr());
        rust_entry5(BUFFER_SIZE5 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
