#![feature(rustc_private, custom_attribute, param_attrs)]
extern crate libc;

extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn memset(_: *mut libc::c_void, _: libc::c_int, _: libc::c_ulong);
    #[no_mangle]
    fn free(_: *mut libc::c_void);
    #[no_mangle]
    fn fabs(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn cosh(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn sinh(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn exp(_: libc::c_double) -> libc::c_double;
}

#[no_mangle]
pub unsafe extern "C" fn ten_mul(mut acc: &mut f64, digit: i32, r: Option<&f64>) -> i32 {
    *acc *= 10i32 as f64;
    *acc += digit as f64;
    *acc += **r.as_ref().unwrap();
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

unsafe fn struct_ptr(mut ctx: Option<&mut Ctx>, mut ctx2: Option<&mut Ctx>, p: Option<&[u8]>) {
    let off = 1;
    (ctx.as_mut().unwrap()).data[0] = p.unwrap()[0 + 3];
    (ctx2.as_mut().unwrap()).data[0] = p.unwrap()[3 + off];
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

#[ownership_mono("", MOVE)]
fn move_ptr(ptr: Option<Box<u32>>) {
    ptr.take();
}

fn attrs(a: &f64, b: &[f64]) -> f64 {
    *a + b[2]
}

#[repr(C)]
#[derive(Clone, Copy)]
struct chacha_ctx {
    input: [u32; 16],
}

unsafe extern "C" fn chacha_ivsetup(mut x: Option<&mut chacha_ctx>, iv: Option<&[u8]>) {
    (x.as_mut().unwrap()).input[12usize] = 0;
    (x.as_mut().unwrap()).input[13usize] = 0;
    (x.as_mut().unwrap()).input[14usize] = iv.unwrap()[0 + 0] as u32
        | (iv.unwrap()[0 + 1] as u32) << 8i32
        | (iv.unwrap()[0 + 2] as u32) << 16i32
        | (iv.unwrap()[0 + 3] as u32) << 24i32;
    (x.as_mut().unwrap()).input[15usize] = iv.unwrap()[4 + 0] as u32
        | (iv.unwrap()[4 + 1] as u32) << 8i32
        | (iv.unwrap()[4 + 2] as u32) << 16i32
        | (iv.unwrap()[4 + 3] as u32) << 24i32;
}

unsafe extern "C" fn void_ptrs(a: *mut libc::c_void,b: *const libc::c_void) {
    let c = a as *mut u32;

    *c = *(b as *const u32);
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct _category {
    pub cat_first: [u8; 4],
    pub delta: libc::c_ushort,
}

impl _category {
    fn cat(&self) -> category {
        0
    }

    fn first(&self) -> u32 {
        0
    }
}

pub type category = libc::c_uint;

unsafe extern "C" fn bisearch_cat(
    ucs: libc::c_uint,
    table: Option<&[_category]>,
    mut max: libc::c_int,
) -> category {
    let mut min: libc::c_int = 0i32;
    let mut mid: libc::c_int = 0;
    if ucs < (table.unwrap()[0]).first()
        || ucs
            > ((table.unwrap()[max as usize]).first() as libc::c_int
                + (table.unwrap()[max as usize]).delta as libc::c_int) as libc::c_uint
    {
        return 4294967295 as category;
    }
    while max >= min {
        mid = (min + max) / 2i32;
        if ucs
            > ((table.unwrap()[mid as usize]).first() as libc::c_int
                + (table.unwrap()[mid as usize]).delta as libc::c_int) as libc::c_uint
        {
            min = mid + 1i32
        } else if ucs < (table.unwrap()[mid as usize]).first() {
            max = mid - 1i32
        } else {
            return (table.unwrap()[mid as usize]).cat();
        }
    }
    return 4294967295 as category;
}

#[no_mangle]
pub unsafe extern "C" fn _cchsh(
    x: libc::c_double,
    mut c: Option<&mut libc::c_double>,
    mut s: Option<&mut libc::c_double>,
) {
    let mut e: libc::c_double = 0.;
    let mut ei: libc::c_double = 0.;
    if fabs(x) <= 0.5f64 {
        **c.as_mut().unwrap() = cosh(x);
        **s.as_mut().unwrap() = sinh(x)
    } else {
        e = exp(x);
        ei = 0.5f64 / e;
        e = 0.5f64 * e;
        **s.as_mut().unwrap() = e - ei;
        **c.as_mut().unwrap() = e + ei
    };
}

#[no_mangle]
pub unsafe extern "C" fn rand_r(mut seed: Option<&mut libc::c_uint>) -> libc::c_int {
    let mut k: libc::c_long = 0;
    let mut s: libc::c_long = **seed.as_mut().unwrap() as libc::c_long;
    if s == 0i32 as libc::c_long {
        s = 0x12345987i32 as libc::c_long
    }
    k = s / 127773i32 as libc::c_long;
    s = 16807i32 as libc::c_long * (s - k * 127773i32 as libc::c_long)
        - 2836i32 as libc::c_long * k;
    if s < 0i32 as libc::c_long {
        s += 2147483647i32 as libc::c_long
    }
    **seed.as_mut().unwrap() = s as libc::c_uint;
    return (s & 0x7fffffffi32 as libc::c_long) as libc::c_int;
}
