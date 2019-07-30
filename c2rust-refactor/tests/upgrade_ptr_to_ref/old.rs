#![feature(rustc_private)]
extern crate libc;

extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
}

#[no_mangle]
pub unsafe extern "C" fn ten_mul(acc: *mut f64, digit: i32, r: *const f64) -> i32 {
    *acc *= 10i32 as f64;
    *acc += digit as f64;
    *acc += *r;
    return 0i32;
}

struct Ctx {
    data: [u8; 10],
}

unsafe fn struct_ptr(ctx: *mut Ctx, ctx2: *mut Ctx, p: *const u8) {
    let off = 1;
    (*ctx).data[0] = *p.offset(0isize).offset(3isize);
    (*ctx2).data[0] = *p.offset(3isize).offset(off);
}

struct Ptrs {
    r: *const u32,
    r2: *mut u32,
    s: *const u32,
    s2: *mut u32,
    boxed: *mut u32,
}

struct SizedData {
    buf: *mut u32,
    bsize: usize,
}

unsafe fn init_buf(sd: *mut SizedData) -> i32 {
    let mut buf: *mut u32 = 0 as *mut u32;

    buf = malloc((*sd).bsize as libc::c_ulong) as *mut u32;

    if buf.is_null() {
        return 1;
    }

    *buf.offset(0) = 1;

    (*sd).buf = buf;

    return 0;
}

unsafe fn init_buf2(sd: *mut SizedData) -> i32 {
    let mut buf: *mut u32 = 0 as *mut u32;

    buf = malloc((*sd).bsize as libc::c_ulong) as *mut u32;

    if buf.is_null() {
        return 1;
    }

    *buf.offset(0) = 1;

    (*sd).buf = buf;

    return 0;
}

use libc::free;

unsafe fn destroy_buf(sd: *mut SizedData) {
    if (*sd).buf.is_null() { return }
    free((*sd).buf as *mut libc::c_void);
    (*sd).buf = 0 as *mut u32;
}

unsafe fn explicit_lifetimes(_ptrs: *mut Ptrs) {}

struct HeapItem {
    item: *mut u32,
    opt_item: *mut u32,
}

unsafe fn init_opt_item(hi: *mut HeapItem) {
    let mut ptr: *mut u32 = 0 as *mut u32;

    if (*hi).item.is_null() {
        return;
    }

    if !(*hi).opt_item.is_null() {
        free((*hi).opt_item as *mut libc::c_void);
    }

    ptr = malloc(32) as *mut u32;
    *ptr = *(*hi).item;

    (*hi).opt_item = ptr;
}
