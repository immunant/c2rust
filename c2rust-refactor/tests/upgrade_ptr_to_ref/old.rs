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

unsafe fn struct_ptr(ctx: *mut Ctx, ctx2: *mut Ctx, p: *mut u8) {
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

    ptr = malloc(4) as *mut u32;

    if ptr.is_null() {
        return
    }

    *ptr = *(*hi).item;

    (*hi).opt_item = ptr;
}

unsafe fn init_opt_item2(hi: *mut HeapItem) {
    let mut ptr: *mut u32 = 0 as *mut u32;

    if (*hi).item.is_null() {
        return;
    }

    if !(*hi).opt_item.is_null() {
        free((*hi).opt_item as *mut libc::c_void);
    }

    ptr = malloc(4) as *mut u32;

    if ptr.is_null() {
        return
    }

    *ptr = *(*hi).item;

    (*hi).opt_item = ptr;
}

use libc::{int32_t, memset, uint16_t, uint32_t};

struct HTab {
    pub hdr: HashHDR,
    pub nmaps: libc::c_int,
    pub mapp: [*mut uint32_t; 32],
}

struct HashHDR {
    pub bsize: int32_t,
    pub bitmaps: [uint16_t; 32],
    pub magic: int32_t,
    pub spares: [int32_t; 32],
}

unsafe fn bm(
    hashp: *mut HTab,
    pnum: libc::c_int,
    nbits: libc::c_int,
    ndx: libc::c_int,
) -> libc::c_int {
    let mut ip: *mut uint32_t = 0 as *mut uint32_t;
    let mut clearbytes: libc::c_int = 0;
    let mut clearints: libc::c_int = 0;

    ip = malloc((*hashp).hdr.bsize as libc::c_ulong) as *mut uint32_t;

    if ip.is_null() {
        return 1i32;
    }

    (*hashp).nmaps += 1;
    clearints = (nbits - 1i32 >> 5i32) + 1i32;
    clearbytes = clearints << 2i32;
    memset(
        ip as *mut libc::c_char as *mut libc::c_void,
        0i32,
        clearbytes as usize,
    );
    memset(
        (ip as *mut libc::c_char).offset(clearbytes as isize) as *mut libc::c_void,
        0xffi32,
        ((*hashp).hdr.bsize - clearbytes) as usize,
    );
    *ip.offset((clearints - 1i32) as isize) = 0xffffffffu32 << (nbits & (1i32 << 5i32) - 1i32);
    let ref mut fresh2 = *ip.offset((0i32 / 32i32) as isize);
    *fresh2 |= (1i32 << 0i32 % 32i32) as libc::c_uint;
    (*hashp).hdr.bitmaps[ndx as usize] = pnum as uint16_t;
    (*hashp).mapp[ndx as usize] = ip;
    return 0i32;
}

unsafe extern "C" fn byteswap(srcp: *mut HashHDR, destp: *mut HashHDR) {
    let mut i: libc::c_int = 0;
    *(&mut (*destp).magic as *mut int32_t as *mut libc::c_char).offset(0isize)
        =
        *(&mut (*srcp).magic as *mut int32_t as
              *mut libc::c_char).offset(3isize);
    *(&mut (*destp).magic as *mut int32_t as *mut libc::c_char).offset(1isize)
        =
        *(&mut (*srcp).magic as *mut int32_t as
              *mut libc::c_char).offset(2isize);
    *(&mut (*destp).magic as *mut int32_t as *mut libc::c_char).offset(2isize)
        =
        *(&mut (*srcp).magic as *mut int32_t as
              *mut libc::c_char).offset(1isize);
    *(&mut (*destp).magic as *mut int32_t as *mut libc::c_char).offset(3isize)
        =
        *(&mut (*srcp).magic as *mut int32_t as
              *mut libc::c_char).offset(0isize);
    i = 0i32;
    while i < 32i32 {
        *(&mut *(*destp).spares.as_mut_ptr().offset(i as isize) as
              *mut int32_t as *mut libc::c_char).offset(0isize) =
            *(&mut *(*srcp).spares.as_mut_ptr().offset(i as isize) as
                  *mut int32_t as *mut libc::c_char).offset(3isize);
        *(&mut *(*destp).spares.as_mut_ptr().offset(i as isize) as
              *mut int32_t as *mut libc::c_char).offset(1isize) =
            *(&mut *(*srcp).spares.as_mut_ptr().offset(i as isize) as
                  *mut int32_t as *mut libc::c_char).offset(2isize);
        *(&mut *(*destp).spares.as_mut_ptr().offset(i as isize) as
              *mut int32_t as *mut libc::c_char).offset(2isize) =
            *(&mut *(*srcp).spares.as_mut_ptr().offset(i as isize) as
                  *mut int32_t as *mut libc::c_char).offset(1isize);
        *(&mut *(*destp).spares.as_mut_ptr().offset(i as isize) as
              *mut int32_t as *mut libc::c_char).offset(3isize) =
            *(&mut *(*srcp).spares.as_mut_ptr().offset(i as isize) as
                  *mut int32_t as *mut libc::c_char).offset(0isize);
        *(&mut *(*destp).bitmaps.as_mut_ptr().offset(i as isize) as
              *mut uint16_t as *mut libc::c_char).offset(0isize) =
            *(&mut *(*srcp).bitmaps.as_mut_ptr().offset(i as isize) as
                  *mut uint16_t as *mut libc::c_char).offset(1isize);
        *(&mut *(*destp).bitmaps.as_mut_ptr().offset(i as isize) as
              *mut uint16_t as *mut libc::c_char).offset(1isize) =
            *(&mut *(*srcp).bitmaps.as_mut_ptr().offset(i as isize) as
                  *mut uint16_t as *mut libc::c_char).offset(0isize);
        i += 1
    };
}

unsafe extern "C" fn byteswap2(hashp: *mut HTab) {
    let mut hdrp: *mut HashHDR = 0 as *mut HashHDR;
    hdrp = &mut (*hashp).hdr;
    let mut _tmp: uint32_t = (*hdrp).magic as uint32_t;
    *(&mut (*hdrp).magic as *mut int32_t as *mut libc::c_char).offset(0isize)
        = *(&mut _tmp as *mut uint32_t as *mut libc::c_char).offset(3isize);
    *(&mut (*hdrp).magic as *mut int32_t as *mut libc::c_char).offset(1isize)
        = *(&mut _tmp as *mut uint32_t as *mut libc::c_char).offset(2isize);
    *(&mut (*hdrp).magic as *mut int32_t as *mut libc::c_char).offset(2isize)
        = *(&mut _tmp as *mut uint32_t as *mut libc::c_char).offset(1isize);
    *(&mut (*hdrp).magic as *mut int32_t as *mut libc::c_char).offset(3isize)
        = *(&mut _tmp as *mut uint32_t as *mut libc::c_char).offset(0isize);
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct _category {
    pub cat_first: [u8; 4],
    pub delta: libc::c_ushort,
}

impl _category {
    fn first(&self) -> libc::c_uint { 0 }
    fn cat(&self) -> Category { 0 }
}

pub type Category = libc::c_uint;

static mut categories: [_category; 2129] = [_category{cat_first: [0; 4], delta: 0,}; 2129];

unsafe extern "C" fn bisearch_cat(
    ucs: libc::c_uint,
    table: *const _category,
    mut max: libc::c_int,
) -> Category {
    let mut min: libc::c_int = 0i32;
    let mut mid: libc::c_int = 0;
    if ucs < (*table.offset(0isize)).first()
        || ucs
            > ((*table.offset(max as isize)).first() as libc::c_int
                + (*table.offset(max as isize)).delta as libc::c_int) as libc::c_uint
    {
        return 4294967295 as Category;
    }
    while max >= min {
        mid = (min + max) / 2i32;
        if ucs
            > ((*table.offset(mid as isize)).first() as libc::c_int
                + (*table.offset(mid as isize)).delta as libc::c_int) as libc::c_uint
        {
            min = mid + 1i32
        } else if ucs < (*table.offset(mid as isize)).first() {
            max = mid - 1i32
        } else {
            return (*table.offset(mid as isize)).cat();
        }
    }
    return 4294967295 as Category;
}

unsafe extern "C" fn opt_params(mut p1: *mut u32, p2: *const u32, p3: *const u32) {
    if p1.is_null() || p2.is_null() {
        return;
    }

    *p1 = *p2 + *p3.offset(0);
}
