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
    #[ownership_static()]
    data: [u8; 10],
}

#[ownership_mono("", WRITE, WRITE, READ)]
#[ownership_constraints(le(WRITE, _0), le(WRITE, _1))]
unsafe fn struct_ptr(mut ctx: Option<&mut Ctx>, mut ctx2: Option<&mut Ctx>, p: Option<&u8>) {
    let off = 1;
    (ctx.as_mut().unwrap()).data[0] = *p.offset(0isize).offset(3isize);
    (ctx2.as_mut().unwrap()).data[0] = *p.offset(3isize).offset(off);
}
