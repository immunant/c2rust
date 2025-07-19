//! extern_crate_c2rust_bitfields

use crate::bitfields::{
    from_csmith, mixed_bitfields, padded_bitfield, rust_compare_three_byte_date, rust_get_bf_ptr,
    rust_init_bitfield_array, rust_init_from_csmith, rust_ma_results, rust_modify_bf_ptr,
    rust_multiple_assignments, rust_ops_padded_bitfield, rust_ops_padded_bitfield_init,
    rust_static_date, rust_two_eight_bits_init, rust_use_renamed_field, rust_write_three_byte_date,
    three_byte_date, two_eight_bits,
};
use std::mem::size_of;

extern "C" {
    fn size_of_three_byte_date() -> usize;
    fn compare_three_byte_date(_: *const three_byte_date, _: u8, _: u8, _: u16) -> u8;
    fn write_three_byte_date(_: *mut three_byte_date, _: u8, _: u8, _: u16);
    fn size_of_padded_bitfield() -> usize;
    fn ops_padded_bitfield(_: *mut padded_bitfield);
    static mut static_date: three_byte_date;
    fn size_of_mixed_bitfields() -> usize;
    fn zeroed_mixed_bitfields() -> mixed_bitfields;
    fn zeroed_padded_bitfield() -> padded_bitfield;
    fn zeroed_three_byte_date() -> three_byte_date;
    fn size_of_from_csmith() -> usize;
    fn size_of_two_eight_bits() -> usize;
    fn multiple_assignments();
    static ma_results: [u8; 17];
}

#[test]
pub fn test_three_byte_date() {
    let c_size_of = unsafe { size_of_three_byte_date() };

    // Ensure correct size (also a packed struct)
    assert_eq!(size_of::<three_byte_date>(), c_size_of);

    // Test zeroed bitfield struct
    let mut tbd = unsafe { zeroed_three_byte_date() };

    tbd.set_day(26);
    tbd.set_month(4);
    tbd.set_year(2019);

    assert_eq!(tbd.day(), 26);
    assert_eq!(tbd.month(), 4);
    assert_eq!(tbd.year(), 2019);

    // Ensure C byte compatibility
    let ret_code = unsafe { compare_three_byte_date(&tbd, 26, 4, 2019) };

    assert_eq!(ret_code, 0);

    // Ensure translated struct ptr reads work
    let rust_ret_code = unsafe { rust_compare_three_byte_date(&tbd, 26, 4, 2019) };

    assert_eq!(rust_ret_code, 0);

    // Ensure can read from C written data
    unsafe { write_three_byte_date(&mut tbd, 24, 12, 2018) }

    assert_eq!(tbd.day(), 24);
    assert_eq!(tbd.month(), 12);
    assert_eq!(tbd.year(), 2018);

    // Ensure translated struct ptr write works
    unsafe { rust_write_three_byte_date(&mut tbd, 17, 7, 1343) }

    assert_eq!(tbd.day(), 17);
    assert_eq!(tbd.month(), 7);
    assert_eq!(tbd.year(), 1343);

    // Test that overflow wraps
    unsafe { rust_write_three_byte_date(&mut tbd, 36, 19, 2u16.pow(15) + 2) }

    assert_eq!(tbd.day(), 4);
    assert_eq!(tbd.month(), 3);
    assert_eq!(tbd.year(), 2);

    // Ensure that C also wraps on overflow
    tbd.set_day(0);
    tbd.set_month(0);
    tbd.set_year(0);

    unsafe { write_three_byte_date(&mut tbd, 36, 19, 2u16.pow(15) + 2) }

    assert_eq!(tbd.day(), 4);
    assert_eq!(tbd.month(), 3);
    assert_eq!(tbd.year(), 2);
}

#[test]
pub fn test_padded_bitfield() {
    let c_size_of = unsafe { size_of_padded_bitfield() };

    assert_eq!(size_of::<padded_bitfield>(), c_size_of);

    // Test zeroed bitfield struct (incl padding)
    let mut pb = unsafe { zeroed_padded_bitfield() };

    pb.set_x(13);

    // Ensure you can apply binary ops on bitfields
    // Through pointers:
    unsafe { ops_padded_bitfield(&mut pb) }

    let c_x = pb.x();

    pb.set_x(13);

    unsafe { rust_ops_padded_bitfield(&mut pb) }

    assert_eq!(pb.x(), c_x);

    let rust_pb = unsafe { rust_ops_padded_bitfield_init() };

    assert_eq!(rust_pb.x(), c_x);
}

#[test]
pub fn test_static_bitfield() {
    // On C static
    unsafe {
        assert_eq!(static_date.day(), 13);

        static_date.set_day(15);

        assert_eq!(static_date.day(), 15);

        rust_write_three_byte_date(&mut static_date, 2, 4, 23);

        assert_eq!(static_date.day(), 2);
        assert_eq!(static_date.month(), 4);
        assert_eq!(static_date.year(), 23);
    }

    // On translated static
    unsafe {
        assert_eq!(rust_static_date.day(), 13);

        rust_static_date.set_day(15);

        assert_eq!(rust_static_date.day(), 15);

        rust_write_three_byte_date(&mut rust_static_date, 2, 4, 23);

        assert_eq!(rust_static_date.day(), 2);
        assert_eq!(rust_static_date.month(), 4);
        assert_eq!(rust_static_date.year(), 23);
    }
}

// Test creating arrays of bitfield structs
// as well as pointers to non bitfields
#[test]
pub fn test_bf_arrays_and_pointers() {
    let c_size_of = unsafe { size_of_mixed_bitfields() };

    assert_eq!(size_of::<mixed_bitfields>(), c_size_of);

    const size: usize = 5;

    // Test zeroed bitfield struct (incl padding)
    let mut array = [unsafe { zeroed_mixed_bitfields() }; size];
    let last_y_ptr = unsafe { rust_init_bitfield_array(array.as_mut_ptr(), size as _) };

    assert_eq!(array[0].x(), 0);
    assert_eq!(array[0].y, 0.0);
    assert_eq!(array[1].x(), 1);
    assert_eq!(array[1].y, 2.2);
    assert_eq!(array[2].x(), 2);
    assert_eq!(array[2].y, 4.4);
    assert_eq!(array[3].x(), 3);
    assert_eq!(array[3].y, 6.6000000000000005);
    assert_eq!(array[4].x(), 4);
    assert_eq!(array[4].y, 8.8);

    unsafe {
        assert_eq!(*last_y_ptr, 8.8);

        *last_y_ptr = 4.4;
    }

    assert_eq!(array[4].y, 4.4);
}

// This a sample struct which was generated by csmith
#[test]
pub fn test_from_csmith() {
    let c_size_of = unsafe { size_of_from_csmith() };

    assert_eq!(size_of::<from_csmith>(), c_size_of);

    let fc = unsafe { rust_init_from_csmith() };

    assert_eq!(fc.f0(), 1);
    assert_eq!(fc.f1(), 2);
    assert_eq!(fc.f2(), 3);
    assert_eq!(fc.f3(), 4);
    assert_eq!(fc.f4(), 5);
    assert_eq!(fc.f5, 6);
    assert_eq!(fc.f6(), 7);
}

#[test]
pub fn test_returned_bitfield_ptr() {
    let ptr = unsafe {
        rust_modify_bf_ptr();
        rust_get_bf_ptr()
    };

    unsafe {
        assert_eq!((*ptr).a(), 2);
    }
}

// Previously there was an issue where the end
// padding was one byte instead of two for this
// struct
#[test]
pub fn test_size_of_two_eight_bits() {
    let c_size_of = unsafe { size_of_two_eight_bits() };

    assert_eq!(size_of::<two_eight_bits>(), c_size_of);
}

#[test]
pub fn test_multiple_assignments() {
    unsafe {
        multiple_assignments();

        assert_eq!(
            ma_results,
            [4, 4, 2, 2, 8, 8, 8, 9, 9, 9, 9, 5, 5, 9, 9, 15, 10]
        );

        rust_multiple_assignments();

        assert_eq!(ma_results, rust_ma_results);
    }
}
