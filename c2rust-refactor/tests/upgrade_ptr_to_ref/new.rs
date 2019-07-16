#[no_mangle]
pub unsafe extern "C" fn ten_mul(acc: &mut f64, digit: i32) -> i32 {
    *acc *= 10i32 as f64;
    *acc += digit as f64;
    return 0i32;
}
