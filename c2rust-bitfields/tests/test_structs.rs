extern crate c2rust_bitfields;
extern crate libc;

use c2rust_bitfields::BitfieldStruct;
use std::mem::{size_of, transmute};
use libc::{c_uchar, c_short, c_ushort, c_ulong, c_uint, c_double};

#[link(name = "test")]
extern "C" {
    fn check_compact_date(_: *const CompactDate) -> c_uint;
    fn assign_compact_date_day(_: *mut CompactDate, _: c_uchar);
    fn check_overlapping_byte_date(_: *const OverlappingByteDate, _: c_ulong, _: c_ushort, _: c_ushort) -> c_uint;
    fn check_unnamed_bitfield(_: *const UnnamedBitfield, _: c_ushort, _: c_ushort, _: c_double) -> c_uint;
    fn check_signed_bitfields(_: *const SignedBitfields, _: c_short, _: c_ushort, _: c_short) -> c_uint;
    fn assign_signed_bitfields(_: *mut SignedBitfields, _: c_short, _: c_ushort, _: c_short);
    fn check_three_byte_date(_: *const ThreeByteDate, _: c_uchar, _: c_uchar, _: c_ushort) -> c_uint;
}

// *** Dumping AST Record Layout
//         0 | struct compact_date
//     0:0-4 |   unsigned char d
//     1:0-3 |   unsigned char m
//         2 |   unsigned short y
//           | [sizeof=4, align=2]
#[repr(C, align(2))]
#[derive(BitfieldStruct, Copy, Clone)]
struct CompactDate {
    // Compact combination of d + m
    // which can't be accessed via ptr in C anyway
    // so we combine the fields into one:
    #[bitfield(name = "d", ty = "libc::c_uchar", bits = "0..=4")]
    #[bitfield(name = "m", ty = "libc::c_uchar", bits = "8..=11")]
    d_m: [u8; 2],
    y: u16,
}

#[test]
fn test_compact_date() {
    assert_eq!(size_of::<CompactDate>(), 4);

    let mut date = CompactDate {
        d_m: [0; 2],
        y: 2014,
    };

    date.set_d(31);
    date.set_m(12);
    date.y = 2014;

    assert_eq!(date.d(), 31);
    assert_eq!(date.m(), 12);
    assert_eq!(date.y, 2014);

    // Test C byte compatibility
    let date_bytes: [u8; 4] = unsafe { transmute(date) };

    assert_eq!(date_bytes, [0b00011111, 0b00001100, 0b11011110, 0b00000111]);
    // 00011111 | 00001100 | 11011110 | 00000111
    //    --31- |     -12- | -2014--> | <--2014-

    unsafe {
        assert_eq!(check_compact_date(&date), 1);
    }
}

#[test]
fn test_compact_date2() {
    let mut date = CompactDate {
        d_m: [0; 2],
        y: 2014,
    };

    date.set_d(14);
    date.set_d(13);

    assert_eq!(date.d(), 13);
    assert_eq!(date.m(), 0);
    assert_eq!(date.y, 2014);

    // Test C byte compatibility
    let date_bytes: [u8; 4] = unsafe { transmute(date) };

    assert_eq!(date_bytes, [0b00001101, 0b00000000, 0b11011110, 0b00000111]);
    // 00001101 | 00000000 | 11011110 | 00000111
    //    --13- |     --m- | -2014--> | <--2014-
}

#[test]
fn test_overflow() {
    let mut date = CompactDate {
        d_m: [0; 2],
        y: 2014,
    };

    date.set_d(31);

    assert_eq!(date.d(), 31);

    date.set_d(32);

    assert_eq!(date.d(), 0);

    date.set_d(255);

    assert_eq!(date.d(), 0);

    // Double check C's overflow
    date.set_d(31);

    assert_eq!(date.d(), 31);

    unsafe {
        assign_compact_date_day(&mut date, 32);
    }

    assert_eq!(date.d(), 0);

    let mut date2 = OverlappingByteDate {
        d_m: [0; 2],
        y: 2019,
        _pad: [0; 4],
    };

    date2.set_d(32);
    date2.set_m(52);

    assert_eq!(date2.d(), 0);
    assert_eq!(date2.m(), 0);
    assert_eq!(date2.y, 2019);

    date2.set_d(14);
    date2.set_m(8);

    date2.set_d(13);
    date2.set_m(52);

    assert_eq!(date2.d(), 13);
    assert_eq!(date2.m(), 0);
    assert_eq!(date2.y, 2019);

    date2.set_d(14);
    date2.set_m(8);

    date2.set_d(45);
    date2.set_m(9);

    assert_eq!(date2.d(), 0);
    assert_eq!(date2.m(), 9);
    assert_eq!(date2.y, 2019);
}

// *** Dumping AST Record Layout
//         0 | struct overlapping_byte_date
//     0:0-4 |   unsigned long d
//     0:5-8 |   unsigned short m
//         2 |   unsigned short y
//           | [sizeof=8, align=8]
#[repr(C, align(8))]
#[derive(BitfieldStruct, Copy, Clone)]
struct OverlappingByteDate {
    // This is also compact, however, the first byte is shared between the two
    // bitfields and the month also has a bit in the second byte
    #[bitfield(name = "d", ty = "libc::c_ulong", bits = "0..=4")]
    #[bitfield(name = "m", ty = "libc::c_ushort", bits = "5..=8")]
    d_m: [u8; 2],
    y: u16,
    _pad: [u8; 4],
}

#[test]
fn test_overlapping_byte_date() {
    assert_eq!(size_of::<OverlappingByteDate>(), 8);

    let mut date = OverlappingByteDate {
        d_m: [0; 2],
        y: 0,
        _pad: [0; 4],
    };

    date.set_d(31);
    date.set_m(12);
    date.y = 2018;

    let date_bytes: [u8; 8] = unsafe { transmute(date) };

    assert_eq!(date_bytes, [0b10011111, 0b00000001, 0b11100010, 0b00000111, 0b0, 0b0, 0b0, 0b0]);
    // 10011111 | 00000001 | 11100010 | 00000111 | 0b0 | 0b0 | 0b0 | 0b0
    // 12/\-31- |        - | -2014--> | <--2014-

    let ret = unsafe {
        check_overlapping_byte_date(&date, 31, 12, 2018)
    };

    assert_eq!(ret, 1);
}

#[test]
fn test_overlapping_byte_date2() {
    let mut date = OverlappingByteDate {
        d_m: [0; 2],
        y: 0,
        _pad: [0; 4],
    };

    date.y = 2019;
    date.set_d(14);
    date.set_m(8);

    assert_eq!(date.d(), 14);
    assert_eq!(date.m(), 8);
    assert_eq!(date.y, 2019);

    let date_bytes: [u8; 8] = unsafe { transmute(date) };

    assert_eq!(date_bytes, [0b00001110, 0b00000001, 0b11100011, 0b00000111, 0b0, 0b0, 0b0, 0b0]);
}

// *** Dumping AST Record Layout
//          0 | struct test
//          0 |   double z
//      8:0-4 |   unsigned short x
//       10:- |   unsigned short
//     10:0-8 |   unsigned short y
//            | [sizeof=16, align=8]
#[repr(C, align(8))]
#[derive(BitfieldStruct, Copy, Clone)]
struct UnnamedBitfield {
    z: f64,
    #[bitfield(name = "x", ty = "libc::c_ushort", bits = "0..=4")]
    x: [u8; 1],
    _pad: [u8; 1],
    #[bitfield(name = "y", ty = "libc::c_ushort", bits = "0..=8")]
    y: [u8; 2],
}

#[test]
fn test_unnamed_bitfield() {
    assert_eq!(size_of::<UnnamedBitfield>(), 16);

    let mut unnamed_bitfield = UnnamedBitfield {
        z: 0.,
        x: [0; 1],
        y: [0; 2],
        _pad: [0; 1],
    };

    unnamed_bitfield.z = 3.14;
    unnamed_bitfield.set_x(30);
    unnamed_bitfield.set_y(505);

    assert_eq!(unnamed_bitfield.x(), 30);
    assert_eq!(unnamed_bitfield.y(), 505);

    let ret = unsafe {
        check_unnamed_bitfield(&unnamed_bitfield, 30, 505, 3.14)
    };

    assert_eq!(ret, 1);
}

// *** Dumping AST Record Layout
//          0 | struct signed_bitfields
//      0:0-3 |   short x
//      0:4-8 |   unsigned short y
//      1:1-5 |   short z
//            | [sizeof=2, align=2]
#[repr(C, align(2))]
#[derive(BitfieldStruct, Copy, Clone)]
struct SignedBitfields {
    #[bitfield(name = "x", ty = "libc::c_short", bits = "0..=3")]
    #[bitfield(name = "y", ty = "libc::c_ushort",bits = "4..=8")]
    #[bitfield(name = "z", ty = "libc::c_short", bits = "9..=13")]
    x_y_z: [u8; 2],
}

#[test]
fn test_signed_bitfields() {
    assert_eq!(size_of::<SignedBitfields>(), 2);

    let mut signed_bitfields = SignedBitfields {
        x_y_z: [0; 2],
    };

    signed_bitfields.set_x(6);
    signed_bitfields.set_y(7);
    signed_bitfields.set_z(13);

    assert_eq!(signed_bitfields.x(), 6);
    assert_eq!(signed_bitfields.y(), 7);
    assert_eq!(signed_bitfields.z(), 13);

    let ret = unsafe {
        check_signed_bitfields(&signed_bitfields, 6, 7, 13)
    };

    assert_eq!(ret, 1);

    signed_bitfields.set_x(-6);
    signed_bitfields.set_y(5);
    signed_bitfields.set_z(-13);

    assert_eq!(signed_bitfields.x(), -6);
    assert_eq!(signed_bitfields.y(), 5);
    assert_eq!(signed_bitfields.z(), -13);

    let bytes: [u8; 2] = unsafe { transmute(signed_bitfields) };

    assert_eq!(bytes, [0b01011010, 0b00100110]);

    let ret = unsafe {
        check_signed_bitfields(&signed_bitfields, -6, 5, -13)
    };

    assert_eq!(ret, 1);
}

#[test]
fn test_signed_underflow_overflow() {
    let mut signed_bitfields = SignedBitfields {
        x_y_z: [0; 2],
    };

    // Overflow
    signed_bitfields.set_x(7);

    assert_eq!(signed_bitfields.x(), 7);

    // Even though 8-15 cannot be represented, it does not count as
    // overflow since they can be represented as negatives
    signed_bitfields.set_x(8);

    assert_eq!(signed_bitfields.x(), -8);

    // C Sanity Check:
    signed_bitfields.set_x(7);

    assert_eq!(signed_bitfields.x(), 7);

    unsafe {
        assign_signed_bitfields(&mut signed_bitfields, 8, 31, 31);
    }

    assert_eq!(signed_bitfields.x(), -8);

    // Values 16+ will still overflow like their unsigned counterparts
    signed_bitfields.set_x(16);

    assert_eq!(signed_bitfields.x(), 0);

    // C Sanity Check:
    signed_bitfields.set_x(7);

    assert_eq!(signed_bitfields.x(), 7);

    unsafe {
        assign_signed_bitfields(&mut signed_bitfields, 16, 31, 31);
    }

    assert_eq!(signed_bitfields.x(), 0);

    // Underflow
    signed_bitfields.set_x(-8);

    assert_eq!(signed_bitfields.x(), -8);

    // However -9 to -15 don't count as underflow
    signed_bitfields.set_x(-9);

    assert_eq!(signed_bitfields.x(), 7);

    // C Sanity Check:
    signed_bitfields.set_x(7);

    assert_eq!(signed_bitfields.x(), 7);

    unsafe {
        assign_signed_bitfields(&mut signed_bitfields, -9, 31, 31);
    }

    assert_eq!(signed_bitfields.x(), 7);

    // Values < -15 will not 0 on underflow apparently
    signed_bitfields.set_x(-31);

    assert_eq!(signed_bitfields.x(), 1);

    // C Sanity Check:
    signed_bitfields.set_x(7);

    assert_eq!(signed_bitfields.x(), 7);

    unsafe {
        assign_signed_bitfields(&mut signed_bitfields, -31, 31, 31);
    }

    assert_eq!(signed_bitfields.x(), 1);
}

// *** Dumping AST Record Layout
//          0 | struct single_bits
//      0:0-0 |   unsigned short x
//      0:1-1 |   short y
//            | [sizeof=2, align=2]
#[repr(C, align(2))]
#[derive(BitfieldStruct, Copy, Clone)]
struct SingleBits {
    #[bitfield(name = "x", ty = "libc::c_ushort", bits = "0..=0")]
    #[bitfield(name = "y", ty = "libc::c_short",bits = "1..=1")]
    x_y: [u8; 1],
    _pad: [u8; 1],
}

#[test]
fn test_single_bits() {
    assert_eq!(size_of::<SingleBits>(), 2);

    let mut single_bits = SingleBits {
        x_y: [0; 1],
        _pad: [0; 1],
    };

    single_bits.set_y(0);

    assert_eq!(single_bits.y(), 0);

    single_bits.set_y(1);

    assert_eq!(single_bits.y(), -1);

    single_bits.set_y(1);

    assert_eq!(single_bits.y(), -1);

    single_bits.set_y(2);

    assert_eq!(single_bits.y(), 0);

    single_bits.set_x(0);

    assert_eq!(single_bits.x(), 0);

    single_bits.set_x(1);

    assert_eq!(single_bits.x(), 1);

    single_bits.set_x(2);

    assert_eq!(single_bits.x(), 0);
}

// *** Dumping AST Record Layout
//          0 | struct three_byte_date
//      0:0-4 |   unsigned char d
//      0:5-8 |   unsigned char m
//     1:1-15 |   unsigned short y
//            | [sizeof=3, align=1]
#[repr(C, align(1))]
#[derive(BitfieldStruct)]
struct ThreeByteDate {
    #[bitfield(name = "day", ty = "libc::c_uchar", bits = "0..=4")]
    #[bitfield(name = "month", ty = "libc::c_uchar", bits = "5..=8")]
    #[bitfield(name = "year", ty = "libc::c_ushort", bits = "9..=23")]
    day_month_year: [u8; 3]
}

#[test]
fn test_three_byte_date() {
    let mut date = ThreeByteDate {
        day_month_year: [0; 3]
    };

    date.set_day(18);
    date.set_month(7);
    date.set_year(2000);

    assert_eq!(date.day_month_year, [0b11110010, 0b10100000, 0b00001111], "{:?}", date.day_month_year);
    assert_eq!(date.day(), 18);
    assert_eq!(date.month(), 7);
    assert_eq!(date.year(), 2000);

    let ret = unsafe {
        check_three_byte_date(&date, 18, 7, 2000)
    };

    assert_eq!(ret, 1);
}
