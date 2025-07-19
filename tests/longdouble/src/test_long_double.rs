//! extern_crate_num_traits

use crate::long_double::{
    rust_cast2double, rust_cast2float, rust_cast2uint, rust_ld1, rust_ld2, rust_long_double_ops,
};
use f128::f128;

#[test]
pub fn test_long_double_ops() {
    let input_result = f128::parse("-4.40000000000000013322676295501878485").unwrap();
    let ret_result = f128::parse("-5.40000000000000013322676295501878485").unwrap();
    let mut input = f128::new(1.7f64);
    let rust_ret = unsafe { rust_long_double_ops(&mut input) };

    assert_eq!(input, input_result);
    assert_eq!(rust_ret, ret_result);
}

#[test]
pub fn test_long_double_casts() {
    let input = f128::parse("4.41234567890123413322676295501878485").unwrap();

    let rust_ret = unsafe { rust_cast2double(input) };

    assert_eq!(rust_ret, 4.412345678901234f64);

    let rust_ret = unsafe { rust_cast2float(input) };

    assert_eq!(rust_ret, 4.412345678901234f32);

    let rust_ret = unsafe { rust_cast2uint(input) };

    assert_eq!(rust_ret, 4u32);
}

#[test]
pub fn test_global_f128s() {
    unsafe {
        assert_eq!(rust_ld1, f128::new(1.0f64));
        assert_eq!(rust_ld2, f128::new(3.0f64));
    }
}
