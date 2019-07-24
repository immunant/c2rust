#![feature(rustc_private)]
extern crate libc;

extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
}

#[no_mangle]
pub unsafe extern "C" fn ten_mul(acc: &mut f64, digit: i32, r: &f64) -> i32 {
    *acc *= 10i32 as f64;
    *acc += digit as f64;
    *acc += *r;
    return 0i32;
}

struct Ctx {
    data: [u8; 10],
}

unsafe fn struct_ptr(ctx: &mut Ctx, ctx2: *mut Ctx, p: &[u8]) {
    let off = 1;
    (ctx).data[0] = p[0 + 3];
    (*ctx2).data[0] = p[3 + off];
}

struct Ptrs<'r, 's> {
    r: &'r u32,
    r2: &'r mut u32,
    s: &'s [u32],
    s2: &'s mut [u32],
    boxed: Option<Box<u32>>,
}

struct SizedData {
    buf: Option<Box<[u32]>>,
    bsize: usize,
}

unsafe fn init_buf(sd: &mut SizedData) -> i32 {
    let mut buf = None;

    buf = Some(
        vec![0; sd.bsize as libc::c_ulong as usize / ::core::mem::size_of::<u32>()]
            .into_boxed_slice(),
    );

    if buf.is_none() {
        return 1;
    }

    buf[0] = 1;

    (sd).buf = buf;

    return 0;
}
