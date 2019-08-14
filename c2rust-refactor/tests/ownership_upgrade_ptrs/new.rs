#![feature(rustc_private, custom_attribute)]
extern crate libc;

extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
}

#[no_mangle]
#[ownership_mono("", WRITE, READ)]
#[ownership_constraints(le(WRITE, _0))]
pub unsafe extern "C" fn ten_mul(acc: &mut f64, digit: i32, r: &f64) -> i32 {
    *acc *= 10i32 as f64;
    *acc += digit as f64;
    *acc += *r;
    return 0i32;
}
