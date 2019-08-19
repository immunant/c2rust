#![feature(rustc_private, custom_attribute)]
extern crate libc;

extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn memset(_: *mut libc::c_void, _: libc::c_int, _: libc::c_ulong);
}

#[no_mangle]
pub unsafe extern "C" fn ten_mul(mut acc: Option<&mut f64>, digit: i32, r: Option<&f64>) -> i32 {
    **acc.as_mut().unwrap() *= 10i32 as f64;
    **acc.as_mut().unwrap() += digit as f64;
    **acc.as_mut().unwrap() += **r.as_ref().unwrap();
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

#[ptr_to_slice(p)]
unsafe fn struct_ptr(mut ctx: Option<&mut Ctx>, mut ctx2: Option<&mut Ctx>, p: Option<&u8>) {
    let off = 1;
    (ctx.as_mut().unwrap()).data[0] = *p.offset(0isize).offset(3isize);
    (ctx2.as_mut().unwrap()).data[0] = *p.offset(3isize).offset(off);
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct HASHHDR {
    pub bsize: libc::c_int,
    pub bitmaps: [libc::c_ushort; 32],
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct HTAB {
    pub hdr: HASHHDR,
    pub mapp: [*mut libc::c_uint; 32],
    pub nmaps: libc::c_int,
}

#[no_mangle]
pub unsafe extern "C" fn __ibitmap(
    mut hashp: Option<&mut HTAB>,
    pnum: libc::c_int,
    nbits: libc::c_int,
    ndx: libc::c_int,
) -> libc::c_int {
    let mut ip: *mut libc::c_uint = 0 as *mut libc::c_uint;
    let mut clearbytes: libc::c_int = 0;
    let mut clearints: libc::c_int = 0;
    ip = malloc((hashp.as_mut().unwrap()).hdr.bsize as libc::c_ulong) as *mut libc::c_uint;
    if ip.is_null() {
        return 1i32;
    }
    (hashp.as_mut().unwrap()).nmaps += 1;
    clearints = (nbits - 1i32 >> 5i32) + 1i32;
    clearbytes = clearints << 2i32;
    memset(
        ip as *mut libc::c_char as *mut libc::c_void,
        0i32,
        clearbytes as libc::c_ulong,
    );
    memset(
        (ip as *mut libc::c_char).offset(clearbytes as isize) as *mut libc::c_void,
        0xffi32,
        ((hashp.as_mut().unwrap()).hdr.bsize - clearbytes) as libc::c_ulong,
    );
    *ip.offset((clearints - 1i32) as isize) = 0xffffffffu32 << (nbits & (1i32 << 5i32) - 1i32);
    let ref mut fresh2 = *ip.offset((0i32 / 32i32) as isize);
    *fresh2 |= (1i32 << 0i32 % 32i32) as libc::c_uint;
    (hashp.as_mut().unwrap()).hdr.bitmaps[ndx as usize] = pnum as libc::c_ushort;
    (hashp.as_mut().unwrap()).mapp[ndx as usize] = ip;
    return 0i32;
}
