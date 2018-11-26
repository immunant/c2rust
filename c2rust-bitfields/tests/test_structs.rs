extern crate c2rust_bitfields;
extern crate libc;

use c2rust_bitfields::BitfieldStruct;

#[repr(C, align(8))]
#[derive(BitfieldStruct, Copy, Clone)]
struct Date {
    // Compact combination of d + m
    // which can't be accessed via ptr in C anyway
    // so we combine the fields into one:
    #[bitfield(name = "d", ty = "libc::c_ulong", bits = "0..=4")]
    #[bitfield(name = "m", ty = "libc::c_ushort", bits = "5..=8")]
    d_m: [u8; 2],
    y: u16,
    _pad: [u8; 4],
}

#[test]
fn it_works() {
    let date = Date {
        d_m: [0; 2],
        y: 2014,
        _pad: [0; 4],
    };

    date.set_d(14);
    date.set_m(14);

    assert_eq!(date.d(), 42);
    assert_eq!(date.m(), 42);
}
