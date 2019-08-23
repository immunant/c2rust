#![feature(rustc_private, custom_attribute, param_attrs)]
extern crate libc;

extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn memset(_: *mut libc::c_void, _: libc::c_int, _: libc::c_ulong);
    #[no_mangle]
    fn free(_: *mut libc::c_void);
}

#[no_mangle]
pub unsafe extern "C" fn ten_mul(#[nonnull] acc: *mut f64, digit: i32, r: *mut f64) -> i32 {
    *acc *= 10i32 as f64;
    *acc += digit as f64;
    *acc += *r;
    return 0i32;
}

#[repr(C)]
#[derive(Copy, Clone)]
struct SizedData {
    buf: *mut u32,
    bsize: usize,
}

#[repr(C)]
#[derive(Copy, Clone)]
struct Ctx {
    data: [u8; 10],
}

unsafe fn struct_ptr(ctx: *mut Ctx, ctx2: *mut Ctx, #[slice] p: *mut u8) {
    let off = 1;
    (*ctx).data[0] = *p.offset(0isize).offset(3isize);
    (*ctx2).data[0] = *p.offset(3isize).offset(off);
}

#[derive ( Copy , Clone )]
#[repr(C)]
pub struct HASHHDR {
    pub bsize: libc::c_int,
    pub bitmaps: [libc::c_ushort; 32],
}

#[derive ( Copy , Clone )]
#[repr(C)]
pub struct HTAB {
    pub hdr: HASHHDR,
    pub mapp: [*mut libc::c_uint; 32],
    pub nmaps: libc::c_int,
}

#[no_mangle]
pub unsafe extern "C" fn __ibitmap(mut hashp: *mut HTAB,
                                   pnum: libc::c_int,
                                   nbits: libc::c_int,
                                   ndx: libc::c_int) -> libc::c_int {
    let mut ip: *mut libc::c_uint = 0 as *mut libc::c_uint;
    let mut clearbytes: libc::c_int = 0;
    let mut clearints: libc::c_int = 0;
    ip = malloc((*hashp).hdr.bsize as libc::c_ulong) as *mut libc::c_uint;
    if ip.is_null() { return 1i32 }
    (*hashp).nmaps += 1;
    clearints = (nbits - 1i32 >> 5i32) + 1i32;
    clearbytes = clearints << 2i32;
    memset(ip as *mut libc::c_char as *mut libc::c_void, 0i32,
           clearbytes as libc::c_ulong);
    memset((ip as *mut libc::c_char).offset(clearbytes as isize) as
               *mut libc::c_void, 0xffi32,
           ((*hashp).hdr.bsize - clearbytes) as libc::c_ulong);
    *ip.offset((clearints - 1i32) as isize) =
        0xffffffffu32 << (nbits & (1i32 << 5i32) - 1i32);
    let ref mut fresh2 = *ip.offset((0i32 / 32i32) as isize);
    *fresh2 |= (1i32 << 0i32 % 32i32) as libc::c_uint;
    (*hashp).hdr.bitmaps[ndx as usize] = pnum as libc::c_ushort;
    (*hashp).mapp[ndx as usize] = ip;
    return 0i32;
}

#[ownership_mono("", MOVE)]
fn move_ptr(ptr: *mut u32) {
    free(ptr as *mut libc::c_void);
}

fn attrs(#[nonnull] a: *const f64, #[nonnull] #[slice] b: *const f64) -> f64 {
    *a + *b.offset(2)
}

#[repr(C)]
#[derive(Clone, Copy)]
struct chacha_ctx {
    input: [u32; 16]
}

unsafe extern "C" fn chacha_ivsetup(mut x: *mut chacha_ctx, #[slice] mut iv: *const u8) {
    (*x).input[12usize] = 0;
    (*x).input[13usize] = 0;
    (*x).input[14usize] = *iv.offset(0isize).offset(0isize) as u32
        | (*iv.offset(0isize).offset(1isize) as u32) << 8i32
        | (*iv.offset(0isize).offset(2isize) as u32) << 16i32
        | (*iv.offset(0isize).offset(3isize) as u32) << 24i32;
    (*x).input[15usize] = *iv.offset(4isize).offset(0isize) as u32
        | (*iv.offset(4isize).offset(1isize) as u32) << 8i32
        | (*iv.offset(4isize).offset(2isize) as u32) << 16i32
        | (*iv.offset(4isize).offset(3isize) as u32) << 24i32;
}

unsafe extern "C" fn void_ptrs(a: *mut libc::c_void,b: *const libc::c_void) {
    let c = a as *mut u32;

    *c = *(b as *const u32);
}
