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
    (ctx).data[0] = p[3];
    (*ctx2).data[0] = p[3 + off];
}
