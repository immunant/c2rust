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

#[derive(Clone)]
struct Ptrs<'r, 's> {
    r: &'r u32,
    r2: &'r mut u32,
    s: &'s [u32],
    s2: &'s mut [u32],
    boxed: Option<Box<u32>>,
}

#[derive(Clone)]
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

    buf.as_mut().unwrap()[0] = 1;

    (sd).buf = buf;

    return 0;
}

unsafe fn init_buf2(sd: &mut SizedData) -> i32 {
    let mut buf;

    buf = vec![0; sd.bsize as libc::c_ulong as usize / ::core::mem::size_of::<u32>()]
        .into_boxed_slice();

    if false {
        return 1;
    }

    buf[0] = 1;

    (sd).buf = Some(buf);

    return 0;
}

use libc::free;

unsafe fn destroy_buf(sd: &mut SizedData) {
    if (sd).buf.is_none() {
        return;
    }
    (sd).buf.take();
    (sd).buf = None;
}

unsafe fn explicit_lifetimes<'a, 'r, 's>(_ptrs: &'a mut Ptrs<'r, 's>) {}

#[derive(Clone)]
struct HeapItem {
    item: Box<u32>,
    opt_item: Option<Box<u32>>,
}

unsafe fn init_opt_item(hi: &mut HeapItem) {
    let mut ptr;

    if false {
        return;
    }

    if !(hi).opt_item.is_none() {
        (hi).opt_item.take();
    }

    ptr = Box::new(0);

    if false {
        return;
    }

    *ptr = *(hi).item;

    (hi).opt_item = Some(ptr);
}

unsafe fn init_opt_item2(hi: &mut HeapItem) {
    let mut ptr = None;

    if false {
        return;
    }

    if !(hi).opt_item.is_none() {
        (hi).opt_item.take();
    }

    ptr = Some(Box::new(0));

    if ptr.is_none() {
        return;
    }

    **ptr.as_mut().unwrap() = *(hi).item;

    (hi).opt_item = ptr;
}

use libc::{int32_t, memset, uint16_t, uint32_t};

#[derive(Clone)]
struct HTab {
    pub hdr: HashHDR,
    pub nmaps: libc::c_int,
    pub mapp: [Option<Box<[uint32_t]>>; 32],
}

struct HashHDR {
    pub bsize: int32_t,
    pub bitmaps: [uint16_t; 32],
    pub magic: int32_t,
    pub spares: [int32_t; 32],
}

unsafe fn bm(
    hashp: &mut HTab,
    pnum: libc::c_int,
    nbits: libc::c_int,
    ndx: libc::c_int,
) -> libc::c_int {
    let mut ip;
    let mut clearbytes: libc::c_int = 0;
    let mut clearints: libc::c_int = 0;

    ip = vec![0; hashp.hdr.bsize as libc::c_ulong as usize / ::core::mem::size_of::<uint32_t>()]
         .into_boxed_slice();

    if false {
        return 1i32;
    }

    (hashp).nmaps += 1;
    clearints = (nbits - 1i32 >> 5i32) + 1i32;
    clearbytes = clearints << 2i32;
    memset(
        ip.as_mut_ptr() as *mut libc::c_char as *mut libc::c_void,
        0i32,
        clearbytes as usize,
    );
    memset(
        (ip.as_mut_ptr() as *mut libc::c_char).offset(clearbytes as isize) as *mut libc::c_void,
        0xffi32,
        ((hashp).hdr.bsize - clearbytes) as usize,
    );
    ip[(clearints - 1i32) as usize] = 0xffffffffu32 << (nbits & (1i32 << 5i32) - 1i32);
    let ref mut fresh2 = ip[(0i32 / 32i32) as usize];
    *fresh2 |= (1i32 << 0i32 % 32i32) as libc::c_uint;
    (hashp).hdr.bitmaps[ndx as usize] = pnum as uint16_t;
    (hashp).mapp[ndx as usize] = Some(ip);
    return 0i32;
}

unsafe extern "C" fn byteswap(srcp: &mut HashHDR, destp: &mut HashHDR) {
    let mut i: libc::c_int = 0;
    (destp).magic = (srcp).magic.swap_bytes();
    i = 0i32;
    while i < 32i32 {
        (destp).spares[i as usize] = (srcp).spares[i as usize].swap_bytes();
        (destp).bitmaps[i as usize] = (srcp).bitmaps[i as usize].swap_bytes();
        i += 1
    }
}

unsafe extern "C" fn byteswap2(hashp: &mut HTab) {
    let mut hdrp;
    hdrp = &mut (hashp).hdr;

    (hdrp).magic = (hdrp).magic.swap_bytes();
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct _category {
    pub cat_first: [u8; 4],
    pub delta: libc::c_ushort,
}

impl _category {
    fn first(&self) -> libc::c_uint {
        0
    }

    fn cat(&self) -> Category {
        0
    }
}

pub type Category = libc::c_uint;

static mut categories: [_category; 2129] = [_category {
    cat_first: [0; 4],
    delta: 0,
}; 2129];

unsafe extern "C" fn bisearch_cat(
    ucs: libc::c_uint,
    table: &[_category],
    mut max: libc::c_int,
) -> Category {
    let mut min: libc::c_int = 0i32;
    let mut mid: libc::c_int = 0;
    if ucs < (table[0]).first()
        || ucs
            > ((table[max as usize]).first() as libc::c_int
                + (table[max as usize]).delta as libc::c_int) as libc::c_uint
    {
        return 4294967295 as Category;
    }
    while max >= min {
        mid = (min + max) / 2i32;
        if ucs
            > ((table[mid as usize]).first() as libc::c_int
                + (table[mid as usize]).delta as libc::c_int) as libc::c_uint
        {
            min = mid + 1i32
        } else if ucs < (table[mid as usize]).first() {
            max = mid - 1i32
        } else {
            return (table[mid as usize]).cat();
        }
    }
    return 4294967295 as Category;
}

unsafe extern "C" fn opt_params(mut p1: Option<&mut u32>, p2: Option<&u32>, p3: Option<&[u32]>) {
    if p1.is_none() || p2.is_none() {
        return;
    }

    **p1.as_mut().unwrap() = *p2.unwrap() + p3.unwrap()[0];
}
