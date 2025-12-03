use crate::big_enum::{rust_entry5, E1, E2, E3};
use crate::enum_as_int::{rust_entry, A, B, E};
use crate::enum_compound::rust_entry6;
use crate::enum_duplicate::{e, rust_entry3};
use crate::enum_fwd_decl::rust_foo;
use crate::enum_ret::{rust_entry2, Color};
use crate::non_canonical_enum_def::{
    hrtimer_restart, rust_abc, HRTIMER_NORESTART, HRTIMER_RESTART,
};
use crate::top_enum::{rust_entry4, E as otherE};

use std::ffi::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    fn entry(_: c_uint, _: *mut c_int);

    fn entry2(_: c_uint, _: *mut c_int);

    fn entry3(_: c_uint, _: *mut c_int);

    fn entry4(_: c_uint, _: *mut c_int);

    fn entry5(_: c_uint, _: *mut c_int);

    fn entry6(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 10;
const BUFFER_SIZE2: usize = 7;
const BUFFER_SIZE3: usize = 4;
const BUFFER_SIZE4: usize = 1;
const BUFFER_SIZE5: usize = 6;
const BUFFER_SIZE6: usize = 1;

#[test]
pub fn test_variants() {
    assert_eq!(A as u32, 0);
    assert_eq!(B as u32, 1);
}

#[test]
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

#[test]
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

#[test]
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

#[test]
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

#[test]
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

#[test]
pub fn test_buffer6() {
    let mut buffer = [0; BUFFER_SIZE6];
    let mut rust_buffer = [0; BUFFER_SIZE6];
    let expected_buffer = [2];

    unsafe {
        entry6(BUFFER_SIZE6 as u32, buffer.as_mut_ptr());
        rust_entry6(BUFFER_SIZE6 as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
