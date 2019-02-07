//! extern_crate_num_traits

extern crate libc;
extern crate f128 as float128;
extern crate num_traits;

use long_double::{rust_long_double_ops, rust_cast2double, rust_cast2float, rust_cast2uint};
use self::float128::f128;
use self::libc::{c_double, c_float, c_uint};
use std::mem::transmute;

pub fn test_long_double_ops() {
    let input_result = f128::parse("-4.40000000000000013322676295501878485").unwrap();
    let ret_result = f128::parse("-5.40000000000000013322676295501878485").unwrap();
    let mut input = f128::new(1.7f64);
    let rust_ret = unsafe {
        rust_long_double_ops(&mut input)
    };

    assert_eq!(input, input_result);
    assert_eq!(rust_ret, ret_result);
}

pub fn test_long_double_casts() {
    let mut input = f128::parse("4.41234567890123413322676295501878485").unwrap();

    let rust_ret = unsafe {
        rust_cast2double(input)
    };

    assert_eq!(rust_ret, 4.412345678901234f64);

    let rust_ret = unsafe {
        rust_cast2float(input)
    };

    assert_eq!(rust_ret, 4.412345678901234f32);

    let rust_ret = unsafe {
        rust_cast2uint(input)
    };

    assert_eq!(rust_ret, 4u32);
}
