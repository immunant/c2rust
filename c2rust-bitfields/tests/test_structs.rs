extern crate c2rust_bitfields;
extern crate libc;

use c2rust_bitfields::BitfieldStruct;
use std::mem::transmute;

// *** Dumping AST Record Layout
//         0 | struct date
//     0:0-4 |   unsigned char d
//     1:0-3 |   unsigned char m
//         2 |   unsigned short y
//           | [sizeof=4, align=2]
// struct CompactDate {
//     unsigned char d: 5;
//     unsigned char m: 4;
//     unsigned short y;
// };

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
    // let mut date = CompactDate {
    //     d_m: [0; 2],
    //     y: 2014,
    // };
    let mut date = CompactDate {
        d_m: [0b00001100, 0b00011111],
        y: 2014,
    };

    date.set_d(14);
    date.set_m(14);
    date.y = 2014;

    assert_eq!(date.d(), 42);
    assert_eq!(date.m(), 42);

    // assert_eq!(date.d(), 31);
    // assert_eq!(date.m(), 12);
    assert_eq!(date.y, 2014);

    // Test byte compatibility: 00000111110111100000110000011111
    let date_bytes: [u8; 4] = unsafe { transmute(date) };

    assert_eq!(date_bytes, [0b00000111, 0b11011110, 0b00001100, 0b00011111]);

    // 00000111 | 11011110 | 00001100 | 00011111
    // -2014--> | <--2014- |     -12- |    --31-
}
