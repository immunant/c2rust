#![feature(rustc_private, custom_attribute, param_attrs, ptr_wrapping_offset_from)]
extern crate libc;

extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    #[ownership_constraints(le(WRITE, _0), le(_0, WRITE))]
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
    #[no_mangle]
    #[ownership_constraints(le(WRITE, _0), le(_0, WRITE))]
    fn takes_ptrs(_: *mut u32, _: *const u32);
    #[no_mangle]
    fn takeswint(_: wint_t) -> wint_t;
    #[no_mangle]
    fn strlen(_: *const libc::c_char) -> size_t;
    #[no_mangle]
    fn memcpy(_: *mut libc::c_void, _: *const libc::c_void, _: size_t)
     -> *mut libc::c_void;
    #[no_mangle]
    fn strdup(_: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn strsep(_: *mut *mut libc::c_char, _: *const libc::c_char)
     -> *mut libc::c_char;
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

unsafe fn struct_ptr(ctx: *mut Ctx, ctx2: *mut Ctx, #[slice] p: *const u8) {
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
pub(crate) struct HTAB {
    pub hdr: HASHHDR,
    #[slice]
    pub mapp: [*mut libc::c_uint; 32],
    pub nmaps: libc::c_int,
}

#[no_mangle]
pub unsafe extern "C" fn __ibitmap(mut hashp: *mut HTAB,
                                   pnum: libc::c_int,
                                   nbits: libc::c_int,
                                   ndx: libc::c_int) -> libc::c_int {
    #[slice]
    #[nonnull]
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

unsafe extern "C" fn chacha_ivsetup(mut x: *mut chacha_ctx, #[slice] iv: *const u8) {
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

#[derive(Clone, Copy)]
#[repr(C)]
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

unsafe extern "C" fn bisearch_cat(ucs: libc::c_uint,
                                  #[slice] table: *const _category,
                                  mut max: libc::c_int) -> category {
    let mut min: libc::c_int = 0i32;
    let mut mid: libc::c_int = 0;
    if ucs < (*table.offset(0isize)).first() ||
           ucs >
               ((*table.offset(max as isize)).first() as libc::c_int +
                    (*table.offset(max as isize)).delta as libc::c_int) as
                   libc::c_uint {
        return 4294967295 as category
    }
    while max >= min {
        mid = (min + max) / 2i32;
        if ucs >
               ((*table.offset(mid as isize)).first() as libc::c_int +
                    (*table.offset(mid as isize)).delta as libc::c_int) as
                   libc::c_uint {
            min = mid + 1i32
        } else if ucs < (*table.offset(mid as isize)).first() {
            max = mid - 1i32
        } else { return (*table.offset(mid as isize)).cat() }
    }
    return 4294967295 as category;
}

#[no_mangle]
pub unsafe extern "C" fn _cchsh(x: libc::c_double,
                                c: *mut libc::c_double,
                                s: *mut libc::c_double) {
    let mut e: libc::c_double = 0.;
    let mut ei: libc::c_double = 0.;
    if fabs(x) <= 0.5f64 {
        *c = cosh(x);
        *s = sinh(x)
    } else {
        e = exp(x);
        ei = 0.5f64 / e;
        e = 0.5f64 * e;
        *s = e - ei;
        *c = e + ei
    };
}

#[no_mangle]
pub unsafe extern "C" fn rand_r(seed: *mut libc::c_uint) -> libc::c_int {
    let mut k: libc::c_long = 0;
    let mut s: libc::c_long = *seed as libc::c_long;
    if s == 0i32 as libc::c_long { s = 0x12345987i32 as libc::c_long }
    k = s / 127773i32 as libc::c_long;
    s =
        16807i32 as libc::c_long * (s - k * 127773i32 as libc::c_long) -
            2836i32 as libc::c_long * k;
    if s < 0i32 as libc::c_long { s += 2147483647i32 as libc::c_long }
    *seed = s as libc::c_uint;
    return (s & 0x7fffffffi32 as libc::c_long) as libc::c_int;
}

fn offset_assign_is_mut(#[slice] z: *mut u8) {
    *z.offset(0) = 1;
    *z.offset(1) = 1;
}

unsafe fn decay_calls(
    cp: *const u32,
    mp: *mut u32,
    #[slice] cs: *const u32,
    #[slice] ms: *mut u32,
    #[nonnull] nncp: *const u32,
    #[nonnull] nnmp: *mut u32,
    #[nonnull] #[slice] nncs: *const u32,
    #[nonnull] #[slice] nnms: *mut u32,
) {
    *mp = 1;
    *ms.offset(0) = 1;
    *nnmp = 1;
    *nnms.offset(0) = 1;

    takes_ptrs(mp, cp);
    takes_ptrs(ms, cs);
    takes_ptrs(nnmp, nncp);
    takes_ptrs(nnms, nncs);
}

unsafe fn rewritten_calls(
    #[nonnull] nncp: *const u32,
    #[nonnull] nnmp: *mut u32,
    #[nonnull] nnmp2: *mut u32,
    #[nonnull] #[slice] nncs: *const u32,
    #[nonnull] #[slice] nnms: *mut u32,
    #[nonnull] #[slice] nnms2: *mut u32,
) {
    decay_calls(0 as *const u32, 0 as *mut u32, 0 as *const u32, 0 as *mut u32, nncp, nnmp, nncs, nnms);
    decay_calls(nncp, nnmp, nncs, nnms, nncp, nnmp2, nncs, nnms2);
}

static mut categories: [_category; 2129] = [_category{cat_first: [0; 4], delta: 0,}; 2129];

#[no_mangle]
pub unsafe extern "C" fn category(ucs: libc::c_uint) -> category {
    return bisearch_cat(ucs, categories.as_ptr(), (categories.len() - 1) as libc::c_int);
}

pub type u32_0 = libc::c_uint;
pub type u8_0 = libc::c_uchar;

static mut sigma: [libc::c_char; 16] =
    [101, 120, 112, 97, 110, 100, 32, 51, 50, 45, 98, 121, 116, 101, 32, 107];
static mut tau: [libc::c_char; 16] =
    [101, 120, 112, 97, 110, 100, 32, 49, 54, 45, 98, 121, 116, 101, 32, 107];
unsafe extern "C" fn chacha_keysetup(mut x: *mut chacha_ctx,
                                     #[slice] mut k: *const u8_0, kbits: u32_0,
                                     ivbits: u32_0) {
    #[slice]
    let mut constants: *const libc::c_char =
        0 as *const libc::c_char; /* kbits == 128 */
    (*x).input[4] =
        *k.offset(0).offset(0) as u32_0 |
            (*k.offset(0).offset(1) as u32_0) << 8i32 |
            (*k.offset(0).offset(2) as u32_0) << 16i32 |
            (*k.offset(0).offset(3) as u32_0) << 24i32;
    if kbits == 256i32 as libc::c_uint {
        /* recommended */
        k = k.offset(16);
        constants = sigma.as_ptr()
    } else { constants = tau.as_ptr() }
    (*x).input[8] =
        *k.offset(0).offset(0) as u32_0 |
            (*k.offset(0).offset(1) as u32_0) << 8i32 |
            (*k.offset(0).offset(2) as u32_0) << 16i32 |
            (*k.offset(0).offset(3) as u32_0) << 24i32;
    (*x).input[0] =
        *constants.offset(0).offset(0) as u32_0 |
            (*constants.offset(0).offset(1) as u32_0) << 8i32 |
            (*constants.offset(0).offset(2) as u32_0) << 16i32 |
            (*constants.offset(0).offset(3) as u32_0) << 24i32;
}

unsafe extern "C" fn chacha_keysetup2(mut x: *mut chacha_ctx,
                                     #[slice] mut k: *const u8_0, kbits: u32_0,
                                     ivbits: u32_0) {
    #[slice]
    #[nonnull]
    let mut constants: *const libc::c_char =
        0 as *const libc::c_char; /* kbits == 128 */
    (*x).input[4] =
        *k.offset(0).offset(0) as u32_0 |
            (*k.offset(0).offset(1) as u32_0) << 8i32 |
            (*k.offset(0).offset(2) as u32_0) << 16i32 |
            (*k.offset(0).offset(3) as u32_0) << 24i32;
    if kbits == 256i32 as libc::c_uint {
        /* recommended */
        k = k.offset(16);
        constants = sigma.as_ptr()
    } else { constants = tau.as_ptr() }
    (*x).input[8] =
        *k.offset(0).offset(0) as u32_0 |
            (*k.offset(0).offset(1) as u32_0) << 8i32 |
            (*k.offset(0).offset(2) as u32_0) << 16i32 |
            (*k.offset(0).offset(3) as u32_0) << 24i32;
    (*x).input[0] =
        *constants.offset(0).offset(0) as u32_0 |
            (*constants.offset(0).offset(1) as u32_0) << 8i32 |
            (*constants.offset(0).offset(2) as u32_0) << 16i32 |
            (*constants.offset(0).offset(3) as u32_0) << 24i32;
}

pub type size_t = libc::c_ulong;
pub type wchar_t = libc::c_int;

#[no_mangle]
pub unsafe extern "C" fn wmemcmp(#[slice] mut s1: *const wchar_t,
                                 #[slice] mut s2: *const wchar_t, n: size_t)
 -> libc::c_int {
    let mut i: size_t = 0;
    i = 0i32 as size_t;
    while i < n {
        if *s1 != *s2 { return if *s1 > *s2 { 1i32 } else { -1i32 } }
        s1 = s1.offset(1isize);
        s2 = s2.offset(1isize);
        i = i.wrapping_add(1)
    }
    return 0i32;
}

#[no_mangle]
pub unsafe extern "C" fn wmemcmp2(#[nonnull] #[slice] mut s1: *const wchar_t,
                                  #[nonnull] #[slice] mut s2: *const wchar_t, n: size_t)
 -> libc::c_int {
    let mut i: size_t = 0;
    i = 0i32 as size_t;
    while i < n {
        if *s1 != *s2 { return if *s1 > *s2 { 1i32 } else { -1i32 } }
        s1 = s1.offset(1isize);
        s2 = s2.offset(1isize);
        i = i.wrapping_add(1)
    }
    return 0i32;
}

#[no_mangle]
pub unsafe extern "C" fn wcsspn(#[slice] mut s: *const wchar_t,
                                #[slice] mut set: *const wchar_t) -> size_t {
    #[slice]
    let mut p: *const wchar_t = 0 as *const wchar_t;
    #[slice]
    let mut q: *const wchar_t = 0 as *const wchar_t;
    p = s;
    while 0 != *p {
        q = set;
        while 0 != *q { if *p == *q { break ; } q = q.offset(1isize) }
        if 0 == *q { break ; }
        p = p.offset(1isize)
    }
    return p.wrapping_offset_from(s) as libc::c_long as size_t;
}

pub type wint_t = libc::c_uint;

#[no_mangle]
pub unsafe extern "C" fn mycasecmp(#[slice] mut s1: *const wchar_t,
                                   #[slice] mut s2: *const wchar_t)
 -> libc::c_int {
    let mut d: libc::c_int = 0i32;
    loop  {
        #[slice]
        let fresh0: *const wchar_t = s1;
        s1 = s1.offset(1);
        let c1: libc::c_int =
            takeswint(*fresh0 as wint_t) as libc::c_int;
        #[slice]
        let fresh1: *const wchar_t = s2;
        s2 = s2.offset(1);
        let c2: libc::c_int =
            takeswint(*fresh1 as wint_t) as libc::c_int;
        d = c1 - c2;
        if d != 0i32 || c2 == '\u{0}' as i32 { break ; }
    }
    return d;
}

type error_t = libc::c_int;

// TODO: argz is Option<&mut Option<Box<[libc::c_char]>>?
pub unsafe extern "C" fn argz_create_sep(#[slice] mut string: *const libc::c_char,
                                         mut sep: libc::c_int,
                                         mut argz: *mut *mut libc::c_char,
                                         mut argz_len: *mut size_t)
 -> error_t {
    let mut len: libc::c_int = 0i32;
    let mut i: libc::c_int = 0i32;
    let mut num_strings: libc::c_int = 0i32;
    let mut delim: [libc::c_char; 2] = [0; 2];
    let mut running: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut old_running: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut token: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut iter: *mut libc::c_char = 0 as *mut libc::c_char;
    *argz_len = 0i32 as size_t;
    if string.is_null() || *string.offset(0) as libc::c_int == '\u{0}' as i32
       {
        *argz = 0 as *mut libc::c_char;
        return 0i32
    }
    delim[0] = sep as libc::c_char;
    delim[1] = '\u{0}' as i32 as libc::c_char;
    running = strdup(string);
    old_running = running;
    loop  {
        token = strsep(&mut running, delim.as_mut_ptr());
        if token.is_null() { break ; }
        len = strlen(token) as libc::c_int;
        *argz_len =
            (*argz_len as
                 libc::c_ulong).wrapping_add((len + 1i32) as libc::c_ulong) as
                size_t as size_t;
        num_strings += 1
    }
    *argz = malloc(*argz_len) as *mut libc::c_char;
    if (*argz).is_null() { return 12i32 }
    free(old_running as *mut libc::c_void);
    running = strdup(string);
    old_running = running;
    iter = *argz;
    i = 0i32;
    while i < num_strings {
        token = strsep(&mut running, delim.as_mut_ptr());
        len =
            strlen(token).wrapping_add(1i32 as libc::c_ulong) as libc::c_int;
        memcpy(iter as *mut libc::c_void, token as *const libc::c_void,
               len as size_t);
        iter = iter.offset(len as isize);
        i += 1
    }
    free(old_running as *mut libc::c_void);
    return 0i32;
}
