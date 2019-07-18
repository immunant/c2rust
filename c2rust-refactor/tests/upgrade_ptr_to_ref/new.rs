#[no_mangle]
pub unsafe extern "C" fn ten_mul(acc: &mut f64, digit: i32, r: &f64) -> i32 {
    *acc *= 10i32 as f64;
    *acc += digit as f64;
    *acc += *r;
    return 0i32;
}

unsafe fn ptr_to_slice(p: &[u8]) {}

struct Ctx {
    data: [u32; 10],
}

unsafe fn struct_ptr(ctx: &mut Ctx, ctx2: *mut Ctx) {
    (ctx).data[0] = 1;
    (*ctx2).data[0] = 1;
}
