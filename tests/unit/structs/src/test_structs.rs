use crate::structs::{rust_alignment_entry, rust_entry, Aligned8Struct, FieldAlignStruct};
use libc::size_t;
use std::ffi::{c_int, c_uint};
use std::mem::{align_of, size_of};

#[link(name = "test")]
extern "C" {
    fn entry(_: c_uint, _: *mut c_int);
    fn alignment_of_aligned8_struct() -> size_t;
    fn alignment_entry(_: c_uint, _: *mut c_int);
    fn alignment_of_field_align_struct_field() -> size_t;
    fn offset_of_field_align_struct_field() -> size_t;
    fn read_field_align_struct_field(_: *const FieldAlignStruct) -> c_int;
    fn write_field_align_struct_field(_: *mut FieldAlignStruct, _: c_int);
}

const BUFFER_SIZE: usize = 9;
const ALIGNMENT_BUFFER_SIZE: usize = 276;

#[test]
pub fn test_buffer() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [1, 2, 3, 10, 20, 0, 0, 0, 0];

    unsafe {
        entry(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_entry(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

#[test]
pub fn test_alignment() {
    let c_alignment = unsafe { alignment_of_aligned8_struct() };

    assert_eq!(align_of::<Aligned8Struct>(), c_alignment);
}

#[test]
pub fn test_field_alignment() {
    let c_alignment = unsafe { alignment_of_field_align_struct_field() };
    let c_offset = unsafe { offset_of_field_align_struct_field() };

    // `f`'s manual alignment is only reachable in Rust by forcing the whole
    // struct's alignment up to it (Rust has no way to raise a single
    // field's alignment without also inflating its size), so the field's
    // required alignment shows up as the struct's alignment instead.
    assert_eq!(align_of::<FieldAlignStruct>(), c_alignment);
    assert_eq!(offset_of!(FieldAlignStruct, f), c_offset as usize);
    assert_eq!(size_of::<FieldAlignStruct>() % c_alignment, 0);

    let mut s = FieldAlignStruct {
        before: 1,
        c2rust_padding: [0; 15],
        f: 0,
        after: 2,
    };

    // The field's actual runtime address must be aligned to `c_alignment`,
    // regardless of where `s` itself happens to live.
    let field_addr = std::ptr::addr_of!(s.f) as usize;
    assert_eq!(field_addr % c_alignment as usize, 0);

    unsafe {
        write_field_align_struct_field(&mut s, 42);
    }
    assert_eq!(s.f, 42);

    s.f = 99;
    let read_back = unsafe { read_field_align_struct_field(&s) };
    assert_eq!(read_back, 99);
}

#[test]
pub fn test_alignments() {
    let mut buffer = [0; ALIGNMENT_BUFFER_SIZE];
    let mut rust_buffer = [0; ALIGNMENT_BUFFER_SIZE];
    let expected_buffer = [
        16, 8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S1
        11, 1, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S2
        12, 2, 42, 1337, 65537, 0, 2, 4, 0, 0, 0, // S3
        12, 4, 42, 1337, 65537, 0, 2, 4, 0, 0, 0, // S4
        16, 8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S5
        11, 1, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S6
        16, 8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S7
        16, 8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S8
        16, 8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S9
        16, 8, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S10
        16, 16, 42, 1337, 65537, 0, 2, 8, 0, 0, 0, // S11
        11, 1, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S12
        12, 2, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S13
        12, 4, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S14
        16, 8, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S15
        16, 16, 42, 1337, 65537, 0, 1, 3, 0, 0, 0, // S16
        24, 8, 8, 10, 16, // S17
        24, 8, 8, 10, 16, // S18
        24, 8, 8, 10, 16, // S19
        24, 8, 8, 10, 16, // S20
        32, 16, 16, 18, 24, // S21
        12, 1, 1, 2, 4, // S22
        14, 2, 2, 3, 5, // S23
        16, 4, 4, 5, 7, // S24
        24, 8, 8, 9, 11, // S25
        32, 16, 16, 17, 19, // S26
        17, 1, 1, 3, 9, // S27
        17, 1, 1, 3, 9, // S28
        17, 1, 1, 3, 9, // S29
        17, 1, 1, 3, 9, // S30
        17, 1, 1, 3, 9, // S31
        12, 1, 1, 2, 4, // S32
        13, 1, 1, 2, 4, // S33
        13, 1, 1, 2, 4, // S34
        17, 1, 1, 2, 4, // S35
        17, 1, 1, 2, 4, // S36
    ];

    unsafe {
        alignment_entry(ALIGNMENT_BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_alignment_entry(ALIGNMENT_BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(&buffer[..], &rust_buffer[..]);
    assert_eq!(&buffer[..], &expected_buffer[..]);
}
