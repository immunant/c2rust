use crate::uninitialized::{bar, baz, e, foo, rust_entry2, s, /*myint, myintp,*/ u};
use std::ffi::{c_int, c_uint};

extern "C" {
    fn entry2(_: c_uint, _: *mut c_int);
}

const BUFFER_SIZE: usize = 1;

#[test]
pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [1];

    unsafe {
        entry2(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry2(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

#[test]
pub fn test_types() {
    assert_eq!(foo as u32, 1);
    assert_eq!(bar as u32, 2);
    assert_eq!(baz as u32, 3);

    // FIXME: union fields are private
    // let my_union = u { x: 32 };

    // let my_struct = s {
    //     a_u: my_union,
    //     a_c: 1,
    //     a_e: e::foo,
    // };
}
