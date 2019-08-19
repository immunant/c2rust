#![feature(rustc_private, custom_attribute)]
extern crate libc;

extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
}

#[no_mangle]
pub unsafe extern "C" fn ten_mul(acc: *mut f64, digit: i32, r: *mut f64) -> i32 {
    *acc *= 10i32 as f64;
    *acc += digit as f64;
    *acc += *r;
    return 0i32;
}

#[repr(C)]
#[derive(Copy, Clone)]
struct SizedData {
    #[ownership_static(READ)]
    buf: *mut u32,
    #[ownership_static()]
    bsize: usize,
}

#[repr(C)]
#[derive(Copy, Clone)]
struct Ctx {
    data: [u8; 10],
}

unsafe fn struct_ptr(ctx: *mut Ctx, ctx2: *mut Ctx, p: *mut u8) {
    let off = 1;
    (*ctx).data[0] = *p.offset(0isize).offset(3isize);
    (*ctx2).data[0] = *p.offset(3isize).offset(off);
}
