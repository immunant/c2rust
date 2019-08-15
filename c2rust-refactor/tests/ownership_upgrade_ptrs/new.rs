#![feature(rustc_private, custom_attribute)]
extern crate libc;

extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
}

#[no_mangle]
#[ownership_mono("", WRITE, READ)]
#[ownership_constraints(le(WRITE, _0))]
pub unsafe extern "C" fn ten_mul(mut acc: Option<&mut f64>, digit: i32, r: Option<&f64>) -> i32 {
    **acc.as_mut().unwrap() *= 10i32 as f64;
    **acc.as_mut().unwrap() += digit as f64;
    **acc.as_mut().unwrap() += **r.as_ref().unwrap();
    return 0i32;
}

struct SizedData {
    #[ownership_static(READ)]
    buf: *mut u32,
    #[ownership_static()]
    bsize: usize,
}
