//! link_in_bitfields_crate

use std::mem::size_of;
use bitfields::{
    three_byte_date, rust_compare_three_byte_date, rust_write_three_byte_date, padded_bitfield,
    rust_ops_padded_bitfield, rust_ops_padded_bitfield_init,
};

extern "C" {
    #[no_mangle]
    fn size_of_three_byte_date() -> usize;
    #[no_mangle]
    fn compare_three_byte_date(_: *const three_byte_date, _: u8, _: u8, _: u16) -> u8;
    #[no_mangle]
    fn write_three_byte_date(_: *mut three_byte_date, _: u8, _: u8, _: u16);
    #[no_mangle]
    fn size_of_padded_bitfield() -> usize;
    #[no_mangle]
    fn ops_padded_bitfield(_: *mut padded_bitfield);
    #[no_mangle]
    static static_date: three_byte_date;
}

pub fn test_three_byte_date() {
    let c_size_of = unsafe {
        size_of_three_byte_date()
    };

    // Ensure correct size (also a packed struct)
    assert_eq!(size_of::<three_byte_date>(), c_size_of);

    // FIXME: get init from translated rust
    let mut tbd = three_byte_date {
        day_month_year: [0; 3],
    };

    tbd.set_day(26);
    tbd.set_month(4);
    tbd.set_year(2019);

    assert_eq!(tbd.day(), 26);
    assert_eq!(tbd.month(), 4);
    assert_eq!(tbd.year(), 2019);

    // Ensure C byte compatibility
    let ret_code = unsafe {
        compare_three_byte_date(&tbd, 26, 4, 2019)
    };

    assert_eq!(ret_code, 0);

    // Ensure translated struct ptr reads work
    let rust_ret_code = unsafe {
        rust_compare_three_byte_date(&tbd, 26, 4, 2019)
    };

    assert_eq!(rust_ret_code, 0);

    // Ensure can read from C written data
    unsafe {
        write_three_byte_date(&mut tbd, 24, 12, 2018)
    }

    assert_eq!(tbd.day(), 24);
    assert_eq!(tbd.month(), 12);
    assert_eq!(tbd.year(), 2018);

    // Ensure translated struct ptr write works
    unsafe {
        rust_write_three_byte_date(&mut tbd, 17, 7, 1343)
    }

    assert_eq!(tbd.day(), 17);
    assert_eq!(tbd.month(), 7);
    assert_eq!(tbd.year(), 1343);

    // Test that overflow wraps
    unsafe {
        rust_write_three_byte_date(&mut tbd, 36, 19, 2u16.pow(15) + 2)
    }

    assert_eq!(tbd.day(), 4);
    assert_eq!(tbd.month(), 3);
    assert_eq!(tbd.year(), 2);

    // Ensure that C also wraps on overflow
    tbd.set_day(0);
    tbd.set_month(0);
    tbd.set_year(0);

    unsafe {
        write_three_byte_date(&mut tbd, 36, 19, 2u16.pow(15) + 2)
    }

    assert_eq!(tbd.day(), 4);
    assert_eq!(tbd.month(), 3);
    assert_eq!(tbd.year(), 2);
}

pub fn test_padded_bitfield() {
    let c_size_of = unsafe {
        size_of_padded_bitfield()
    };

    assert_eq!(size_of::<padded_bitfield>(), c_size_of);

    // FIXME: get init from translated rust
    let mut pb = padded_bitfield {
        x: [0; 1],
        _pad: [0; 1],
        z: [0; 2],
        _pad2: [0; 4],
    };

    pb.set_x(13);

    // Ensure you can apply binary ops on bitfields
    // Through pointers:
    unsafe {
        ops_padded_bitfield(&mut pb)
    }

    let c_x = pb.x();

    pb.set_x(13);

    unsafe {
        rust_ops_padded_bitfield(&mut pb)
    }

    assert_eq!(pb.x(), c_x);

    let rust_pb = unsafe {
        rust_ops_padded_bitfield_init()
    };

    assert_eq!(rust_pb.x(), c_x);
}

pub fn test_static_bitfield() {
    unsafe {
        assert_eq!(static_date.day(), 13);
    }

    unsafe {
        static_date.set_day(12);
    }
}
