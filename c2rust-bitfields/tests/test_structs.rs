extern crate c2rust_bitfields;
extern crate libc;

use c2rust_bitfields::BitfieldStruct;
use std::mem::{size_of, transmute};
use libc::{c_uchar, c_ushort, c_ulong, c_uint};

#[link(name = "test")]
extern "C" {
    fn check_compact_date(_: *const CompactDate) -> c_uint;
    fn assign_compact_date_day(_: *mut CompactDate, _: c_uchar);
    fn check_overlapping_byte_date(_: *const OverlappingByteDate, _: c_ulong, _: c_ushort, _: c_ushort) -> c_uint;
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
    #[bitfield(name = "d", ty = "libc::c_ulong", bits = "0..=4")]
    #[bitfield(name = "m", ty = "libc::c_ushort", bits = "8..=11")]
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

    // assert_eq!(date.d(), 13); // 15
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
