#![feature(
    rustc_private,
    c_variadic,
    extern_types,
    register_tool,
    register_attr
)]
#![register_tool(c2rust)]
#![register_attr(slice, nonnull, ownership_constraints)]

extern "C" {
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[ownership_constraints(le(WRITE, _0), le(_0, WRITE))]
    fn memset(_: *mut libc::c_void, _: libc::c_int, _: libc::c_ulong);
    fn free(_: *mut libc::c_void);
    fn fabs(_: libc::c_double) -> libc::c_double;
    fn cosh(_: libc::c_double) -> libc::c_double;
    fn sinh(_: libc::c_double) -> libc::c_double;
    fn exp(_: libc::c_double) -> libc::c_double;
    #[ownership_constraints(le(WRITE, _0), le(_0, WRITE))]
    fn takes_ptrs(_: *mut u32, _: *const u32);
    fn takeswint(_: wint_t) -> wint_t;
    fn strlen(_: *const libc::c_char) -> size_t;
    fn memcpy(_: *mut libc::c_void, _: *const libc::c_void, _: size_t) -> *mut libc::c_void;
    fn strdup(_: *const libc::c_char) -> *mut libc::c_char;
    fn strsep(_: *mut *mut libc::c_char, _: *const libc::c_char) -> *mut libc::c_char;
    fn __chk_fail() -> !;
    fn vsprintf(_: *mut libc::c_char, _: *const libc::c_char, _: ::std::ffi::VaList)
        -> libc::c_int;
    fn vsnprintf(
        _: *mut libc::c_char,
        _: size_t,
        _: *const libc::c_char,
        _: ::std::ffi::VaList,
    ) -> libc::c_int;
    fn get_ptr() -> *mut u32;
    fn get_struct_ptr() -> *mut Ctx;
    type _reent;
    static mut _impure_ptr: *mut _reent;
}

pub unsafe extern "C" fn ten_mul(#[nonnull] mut acc: &mut f64, digit: i32, r: Option<&f64>) -> i32 {
    *acc *= 10i32 as f64;
    *acc += digit as f64;
    *acc += *r.unwrap();
    return 0i32;
}

#[repr(C)]
#[derive(Clone)]
struct SizedData {
    #[slice]
    buf: Option<Box<[u32]>>,
    bsize: usize,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Ctx {
    pub data: [u8; 5],
}

pub unsafe fn struct_ptr(
    mut ctx: Option<&mut Ctx>,
    mut ctx2: Option<&mut Ctx>,
    #[slice] p: Option<&[u8]>,
) {
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
#[derive(Clone)]
pub(crate) struct HTAB {
    pub hdr: HASHHDR,
    #[slice]
    pub mapp: [Option<Box<[libc::c_uint]>>; 32],
    pub nmaps: libc::c_int,
}

pub(crate) unsafe extern "C" fn __ibitmap(
    mut hashp: Option<&mut HTAB>,
    pnum: libc::c_int,
    nbits: libc::c_int,
    ndx: libc::c_int,
) -> libc::c_int {
    #[slice]
    #[nonnull]
    let mut ip;
    let mut clearbytes: libc::c_int = 0;
    let mut clearints: libc::c_int = 0;
    ip = vec![
        0;
        hashp.as_mut().unwrap().hdr.bsize as libc::c_ulong as usize
            / ::core::mem::size_of::<libc::c_uint>()
    ]
    .into_boxed_slice();
    if false {
        return 1i32;
    }
    (hashp.as_mut().unwrap()).nmaps += 1;
    clearints = (nbits - 1i32 >> 5i32) + 1i32;
    clearbytes = clearints << 2i32;
    memset(
        ip.as_mut_ptr() as *mut libc::c_char as *mut libc::c_void,
        0i32,
        clearbytes as libc::c_ulong,
    );
    memset(
        (ip.as_mut_ptr() as *mut libc::c_char).offset(clearbytes as isize) as *mut libc::c_void,
        0xffi32,
        ((hashp.as_mut().unwrap()).hdr.bsize - clearbytes) as libc::c_ulong,
    );
    ip[(clearints - 1i32) as usize] = 0xffffffffu32 << (nbits & (1i32 << 5i32) - 1i32);
    let ref mut fresh2 = ip[(0i32 / 32i32) as usize];
    *fresh2 |= (1i32 << 0i32 % 32i32) as libc::c_uint;
    (hashp.as_mut().unwrap()).hdr.bitmaps[ndx as usize] = pnum as libc::c_ushort;
    (hashp.as_mut().unwrap()).mapp[ndx as usize] = Some(ip);
    return 0i32;
}

pub(crate) unsafe extern "C" fn __ibitmap2(
    mut hashp: Option<&mut HTAB>,
    pnum: libc::c_int,
    nbits: libc::c_int,
    ndx: libc::c_int,
) -> libc::c_int {
    #[slice]
    let mut ip = None;
    let mut clearbytes: libc::c_int = 0;
    let mut clearints: libc::c_int = 0;
    ip = Some(
        vec![
            0;
            hashp.as_mut().unwrap().hdr.bsize as libc::c_ulong as usize
                / ::core::mem::size_of::<libc::c_uint>()
        ]
        .into_boxed_slice(),
    );
    if ip.is_none() {
        return 1i32;
    }
    (hashp.as_mut().unwrap()).nmaps += 1;
    clearints = (nbits - 1i32 >> 5i32) + 1i32;
    clearbytes = clearints << 2i32;
    memset(
        ip.as_mut().map(|r| r.as_mut_ptr()).unwrap_or(0 as *mut _) as *mut libc::c_char
            as *mut libc::c_void,
        0i32,
        clearbytes as libc::c_ulong,
    );
    memset(
        (ip.as_mut().map(|r| r.as_mut_ptr()).unwrap_or(0 as *mut _) as *mut libc::c_char)
            .offset(clearbytes as isize) as *mut libc::c_void,
        0xffi32,
        ((hashp.as_mut().unwrap()).hdr.bsize - clearbytes) as libc::c_ulong,
    );
    ip.as_mut().unwrap()[(clearints - 1i32) as usize] =
        0xffffffffu32 << (nbits & (1i32 << 5i32) - 1i32);
    let ref mut fresh2 = ip.as_mut().unwrap()[(0i32 / 32i32) as usize];
    *fresh2 |= (1i32 << 0i32 % 32i32) as libc::c_uint;
    (hashp.as_mut().unwrap()).hdr.bitmaps[ndx as usize] = pnum as libc::c_ushort;
    (hashp.as_mut().unwrap()).mapp[ndx as usize] = ip;
    return 0i32;
}

unsafe fn move_ptr(mut ptr: Option<Box<u32>>) {
    ptr.take();
}

unsafe fn attrs(
    #[nonnull] a: &f64,
    #[nonnull]
    #[slice]
    b: &[f64],
) -> f64 {
    *a + b[2]
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct chacha_ctx {
    input: [u32; 16],
}

unsafe extern "C" fn chacha_ivsetup(mut x: Option<&mut chacha_ctx>, #[slice] iv: Option<&[u8]>) {
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

unsafe extern "C" fn void_ptrs(a: *mut libc::c_void, b: *const libc::c_void) {
    #[nonnull]
    let c: *mut u32 = a as *mut u32;

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
    #[slice] table: Option<&[_category]>,
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

unsafe fn offset_assign_is_mut(#[slice] mut z: Option<&mut [u8]>) {
    z.as_mut().unwrap()[0] = 1;
    z.as_mut().unwrap()[1] = 1;
}

unsafe fn decay_calls(
    cp: Option<&u32>,
    mut mp: Option<&mut u32>,
    #[slice] cs: Option<&[u32]>,
    #[slice] mut ms: Option<&mut [u32]>,
    #[nonnull] nncp: &u32,
    #[nonnull] mut nnmp: &mut u32,
    #[nonnull]
    #[slice]
    nncs: &[u32],
    #[nonnull]
    #[slice]
    mut nnms: &mut [u32],
) {
    **mp.as_mut().unwrap() = 1;
    ms.as_mut().unwrap()[0] = 1;
    *nnmp = 1;
    nnms[0] = 1;

    takes_ptrs(
        mp.as_mut()
            .map(|r| &mut **r as *mut _)
            .unwrap_or(0 as *mut _),
        cp.as_ref()
            .map(|r| &**r as *const _)
            .unwrap_or(0 as *const _),
    );
    takes_ptrs(
        ms.as_mut().map(|r| r.as_mut_ptr()).unwrap_or(0 as *mut _),
        cs.as_ref().map(|r| r.as_ptr()).unwrap_or(0 as *const _),
    );
    takes_ptrs(nnmp, nncp);
    takes_ptrs(nnms.as_mut_ptr(), nncs.as_ptr());
}

unsafe fn rewritten_calls(
    #[nonnull] nncp: &u32,
    #[nonnull] mut nnmp: &mut u32,
    #[nonnull] mut nnmp2: &mut u32,
    #[nonnull]
    #[slice]
    nncs: &[u32],
    #[nonnull]
    #[slice]
    mut nnms: &mut [u32],
    #[nonnull]
    #[slice]
    mut nnms2: &mut [u32],
) {
    decay_calls(None, None, None, None, nncp, nnmp, nncs, nnms);
    decay_calls(
        Some(nncp),
        Some(nnmp),
        Some(nncs),
        Some(nnms),
        nncp,
        nnmp2,
        nncs,
        nnms2,
    );
}

static mut categories: [_category; 2129] = [_category {
    cat_first: [0; 4],
    delta: 0,
}; 2129];

pub unsafe extern "C" fn category(ucs: libc::c_uint) -> category {
    return bisearch_cat(
        ucs,
        Some(&categories),
        (categories.len() - 1) as libc::c_int,
    );
}

pub type u32_0 = libc::c_uint;
pub type u8_0 = libc::c_uchar;

static mut sigma: [libc::c_char; 16] = [
    101, 120, 112, 97, 110, 100, 32, 51, 50, 45, 98, 121, 116, 101, 32, 107,
];
static mut tau: [libc::c_char; 16] = [
    101, 120, 112, 97, 110, 100, 32, 49, 54, 45, 98, 121, 116, 101, 32, 107,
];
pub unsafe extern "C" fn chacha_keysetup(
    mut x: Option<&mut chacha_ctx>,
    #[slice] mut k: Option<&[u8_0]>,
    kbits: u32_0,
    _ivbits: u32_0,
) {
    #[slice]
    let mut constants = None; /* kbits == 128 */
    (x.as_mut().unwrap()).input[4] = k.unwrap()[0 + 0] as u32_0
        | (k.unwrap()[0 + 1] as u32_0) << 8i32
        | (k.unwrap()[0 + 2] as u32_0) << 16i32
        | (k.unwrap()[0 + 3] as u32_0) << 24i32;
    if kbits == 256i32 as libc::c_uint {
        /* recommended */
        k = Some(&k.unwrap()[16..]);
        constants = Some(&sigma)
    } else {
        constants = Some(&tau)
    }
    (x.as_mut().unwrap()).input[8] = k.unwrap()[0 + 0] as u32_0
        | (k.unwrap()[0 + 1] as u32_0) << 8i32
        | (k.unwrap()[0 + 2] as u32_0) << 16i32
        | (k.unwrap()[0 + 3] as u32_0) << 24i32;
    (x.as_mut().unwrap()).input[0] = constants.unwrap()[0 + 0] as u32_0
        | (constants.unwrap()[0 + 1] as u32_0) << 8i32
        | (constants.unwrap()[0 + 2] as u32_0) << 16i32
        | (constants.unwrap()[0 + 3] as u32_0) << 24i32;
}

pub unsafe extern "C" fn chacha_keysetup2(
    mut x: Option<&mut chacha_ctx>,
    #[slice] mut k: Option<&[u8_0]>,
    kbits: u32_0,
    _ivbits: u32_0,
) {
    #[slice]
    #[nonnull]
    let mut constants; /* kbits == 128 */
    (x.as_mut().unwrap()).input[4] = k.unwrap()[0 + 0] as u32_0
        | (k.unwrap()[0 + 1] as u32_0) << 8i32
        | (k.unwrap()[0 + 2] as u32_0) << 16i32
        | (k.unwrap()[0 + 3] as u32_0) << 24i32;
    if kbits == 256i32 as libc::c_uint {
        /* recommended */
        k = Some(&k.unwrap()[16..]);
        constants = &sigma
    } else {
        constants = &tau
    }
    (x.as_mut().unwrap()).input[8] = k.unwrap()[0 + 0] as u32_0
        | (k.unwrap()[0 + 1] as u32_0) << 8i32
        | (k.unwrap()[0 + 2] as u32_0) << 16i32
        | (k.unwrap()[0 + 3] as u32_0) << 24i32;
    (x.as_mut().unwrap()).input[0] = constants[0 + 0] as u32_0
        | (constants[0 + 1] as u32_0) << 8i32
        | (constants[0 + 2] as u32_0) << 16i32
        | (constants[0 + 3] as u32_0) << 24i32;
}

pub type size_t = libc::c_ulong;
pub type wchar_t = libc::c_int;

pub unsafe extern "C" fn wmemcmp(
    #[slice] mut s1: Option<&[wchar_t]>,
    #[slice] mut s2: Option<&[wchar_t]>,
    n: size_t,
) -> libc::c_int {
    let mut i: size_t = 0;
    i = 0i32 as size_t;
    while i < n {
        if s1.unwrap()[0] != s2.unwrap()[0] {
            return if s1.unwrap()[0] > s2.unwrap()[0] {
                1i32
            } else {
                -1i32
            };
        }
        s1 = Some(&s1.unwrap()[1..]);
        s2 = Some(&s2.unwrap()[1..]);
        i = i.wrapping_add(1)
    }
    return 0i32;
}

pub unsafe extern "C" fn wmemcmp2(
    #[nonnull]
    #[slice]
    mut s1: &[wchar_t],
    #[nonnull]
    #[slice]
    mut s2: &[wchar_t],
    n: size_t,
) -> libc::c_int {
    let mut i: size_t = 0;
    i = 0i32 as size_t;
    while i < n {
        if s1[0] != s2[0] {
            return if s1[0] > s2[0] { 1i32 } else { -1i32 };
        }
        s1 = &s1[1..];
        s2 = &s2[1..];
        i = i.wrapping_add(1)
    }
    return 0i32;
}

pub unsafe extern "C" fn wcsspn(
    #[slice] mut s: Option<&[wchar_t]>,
    #[slice] mut set: Option<&[wchar_t]>,
) -> size_t {
    #[slice]
    let mut p = None;
    #[slice]
    let mut q = None;
    p = s;
    while 0 != p.unwrap()[0] {
        q = set;
        while 0 != q.unwrap()[0] {
            if p.unwrap()[0] == q.unwrap()[0] {
                break;
            }
            q = Some(&q.unwrap()[1..])
        }
        if 0 == q.unwrap()[0] {
            break;
        }
        p = Some(&p.unwrap()[1..])
    }
    return p
        .as_ref()
        .map(|r| r.as_ptr())
        .unwrap_or(0 as *const _)
        .wrapping_offset_from(s.as_ref().map(|r| r.as_ptr()).unwrap_or(0 as *const _))
        as libc::c_long as size_t;
}

pub type wint_t = libc::c_uint;

pub unsafe extern "C" fn mycasecmp(
    #[slice] mut s1: Option<&[wchar_t]>,
    #[slice] mut s2: Option<&[wchar_t]>,
) -> libc::c_int {
    let mut d: libc::c_int = 0i32;
    loop {
        #[slice]
        let fresh0 = s1;
        s1 = Some(&s1.unwrap()[1..]);
        let c1: libc::c_int = takeswint(fresh0.unwrap()[0] as wint_t) as libc::c_int;
        #[slice]
        let fresh1 = s2;
        s2 = Some(&s2.unwrap()[1..]);
        let c2: libc::c_int = takeswint(fresh1.unwrap()[0] as wint_t) as libc::c_int;
        d = c1 - c2;
        if d != 0i32 || c2 == '\u{0}' as i32 {
            break;
        }
    }
    return d;
}

type error_t = libc::c_int;

// TODO: argz is Option<&mut Option<Box<[libc::c_char]>>?
pub unsafe extern "C" fn argz_create_sep(
    #[slice] mut string: Option<&[libc::c_char]>,
    mut sep: libc::c_int,
    mut argz: Option<&mut *mut libc::c_char>,
    mut argz_len: Option<&mut size_t>,
) -> error_t {
    let mut len: libc::c_int = 0i32;
    let mut i: libc::c_int = 0i32;
    let mut num_strings: libc::c_int = 0i32;
    let mut delim: [libc::c_char; 2] = [0; 2];
    let mut running = None;
    let mut old_running = None;
    let mut token = None;
    let mut iter = None;
    **argz_len.as_mut().unwrap() = 0i32 as size_t;
    if string.is_none() || string.unwrap()[0] as libc::c_int == '\u{0}' as i32 {
        **argz.as_mut().unwrap() = 0 as *mut libc::c_char;
        return 0i32;
    }
    delim[0] = sep as libc::c_char;
    delim[1] = '\u{0}' as i32 as libc::c_char;
    running = ::core::ptr::NonNull::new(strdup(
        string.as_ref().map(|r| r.as_ptr()).unwrap_or(0 as *const _),
    ));
    old_running = running;
    loop {
        token = ::core::ptr::NonNull::new(strsep(
            &mut running.map(|r| r.as_ptr()).unwrap_or(0 as *mut _),
            delim.as_mut_ptr(),
        ));
        if token.is_none() {
            break;
        }
        len = strlen(token.map(|r| r.as_ptr()).unwrap_or(0 as *mut _)) as libc::c_int;
        **argz_len.as_mut().unwrap() = (**argz_len.as_mut().unwrap() as libc::c_ulong)
            .wrapping_add((len + 1i32) as libc::c_ulong)
            as size_t as size_t;
        num_strings += 1
    }
    **argz.as_mut().unwrap() = malloc(**argz_len.as_mut().unwrap()) as *mut libc::c_char;
    if (**argz.as_mut().unwrap()).is_null() {
        return 12i32;
    }
    free(
        old_running
            .take()
            .map(|r| r.as_ptr() as *const _)
            .unwrap_or(0 as *const _) as *mut libc::c_void,
    );
    running = ::core::ptr::NonNull::new(strdup(
        string.as_ref().map(|r| r.as_ptr()).unwrap_or(0 as *const _),
    ));
    old_running = running;
    iter = Some(**argz.as_mut().unwrap());
    i = 0i32;
    while i < num_strings {
        token = ::core::ptr::NonNull::new(strsep(
            &mut running.map(|r| r.as_ptr()).unwrap_or(0 as *mut _),
            delim.as_mut_ptr(),
        ));
        len = strlen(token.map(|r| r.as_ptr()).unwrap_or(0 as *mut _))
            .wrapping_add(1i32 as libc::c_ulong) as libc::c_int;
        memcpy(
            iter.as_mut()
                .map(|r| &mut **r as *mut _)
                .unwrap_or(0 as *mut _) as *mut libc::c_void,
            token.map(|r| r.as_ptr()).unwrap_or(0 as *mut _) as *const libc::c_void,
            len as size_t,
        );
        iter = Some(iter.unwrap().offset(len as isize));
        i += 1
    }
    free(
        old_running
            .take()
            .map(|r| r.as_ptr() as *const _)
            .unwrap_or(0 as *const _) as *mut libc::c_void,
    );
    return 0i32;
}

pub unsafe extern "C" fn eisnan(#[slice] mut x: Option<&[libc::c_ushort]>) -> libc::c_int {
    let mut i: libc::c_int = 0;
    /* NaN has maximum exponent */
    if x.unwrap()[(10i32 - 1i32) as usize] as libc::c_int & 0x7fffi32 != 0x7fffi32 {
        return 0i32;
    }
    /* ... and non-zero significand field. */
    i = 0i32;
    while i < 10i32 - 1i32 {
        #[slice]
        let fresh4 = x;
        x = Some(&x.unwrap()[1..]);
        if fresh4.unwrap()[0] as libc::c_int != 0i32 {
            return 1i32;
        }
        i += 1
    }
    return 0i32;
}

pub unsafe extern "C" fn eneg(#[slice] mut x: Option<&mut [libc::c_ushort]>) {
    if eisnan(x.as_ref().map(|r| &**r)) != 0 {
        return;
    }
    let ref mut fresh3 = x.as_mut().unwrap()[(10i32 - 1i32) as usize];
    *fresh3 = (*fresh3 as libc::c_int ^ 0x8000i32) as libc::c_ushort;
    /* Toggle the sign bit */
}

pub unsafe extern "C" fn eneg2(
    #[nonnull]
    #[slice]
    mut x: &mut [libc::c_ushort],
) {
    if eisnan(Some(x)) != 0 {
        return;
    }
    let ref mut fresh3 = x[(10i32 - 1i32) as usize];
    *fresh3 = (*fresh3 as libc::c_int ^ 0x8000i32) as libc::c_ushort;
    /* Toggle the sign bit */
}

pub unsafe extern "C" fn eisnan2(
    #[nonnull]
    #[slice]
    mut x: &[libc::c_ushort],
) -> libc::c_int {
    let mut i: libc::c_int = 0;
    /* NaN has maximum exponent */
    if x[(10i32 - 1i32) as usize] as libc::c_int & 0x7fffi32 != 0x7fffi32 {
        return 0i32;
    }
    /* ... and non-zero significand field. */
    i = 0i32;
    while i < 10i32 - 1i32 {
        #[slice]
        #[nonnull]
        let fresh4 = x;
        x = &x[1..];
        if fresh4[0] as libc::c_int != 0i32 {
            return 1i32;
        }
        i += 1
    }
    return 0i32;
}

pub unsafe extern "C" fn eneg3(
    #[nonnull]
    #[slice]
    mut x: &mut [libc::c_ushort],
) {
    if eisnan2(x) != 0 {
        return;
    }
    let ref mut fresh3 = x[(10i32 - 1i32) as usize];
    *fresh3 = (*fresh3 as libc::c_int ^ 0x8000i32) as libc::c_ushort;
    /* Toggle the sign bit */
}

pub unsafe extern "C" fn eshdn1(#[slice] mut x: Option<&mut [libc::c_ushort]>) {
    let mut bits: libc::c_ushort = 0; /* point to significand area */
    let mut i: libc::c_int = 0;
    x = Some(x.unwrap().split_at_mut(2).1);
    bits = 0i32 as libc::c_ushort;
    i = 2i32;
    while i < 10i32 + 3i32 {
        if x.as_mut().unwrap()[0] as libc::c_int & 1i32 != 0 {
            bits = (bits as libc::c_int | 1i32) as libc::c_ushort
        }

        x.as_mut().unwrap()[0] = (x.as_mut().unwrap()[0] as libc::c_int >> 1i32) as libc::c_ushort;
        if bits as libc::c_int & 2i32 != 0 {
            x.as_mut().unwrap()[0] =
                (x.as_mut().unwrap()[0] as libc::c_int | 0x8000i32) as libc::c_ushort
        }
        bits = ((bits as libc::c_int) << 1i32) as libc::c_ushort;
        x = Some(x.unwrap().split_at_mut(1).1);
        i += 1
    }
}

pub unsafe extern "C" fn eshdn1_nonnull(
    #[nonnull]
    #[slice]
    mut x: &mut [libc::c_ushort],
) {
    let mut bits: libc::c_ushort = 0; /* point to significand area */
    let mut i: libc::c_int = 0;
    x = x.split_at_mut(2).1;
    bits = 0i32 as libc::c_ushort;
    i = 2i32;
    while i < 10i32 + 3i32 {
        if x[0] as libc::c_int & 1i32 != 0 {
            bits = (bits as libc::c_int | 1i32) as libc::c_ushort
        }

        x[0] = (x[0] as libc::c_int >> 1i32) as libc::c_ushort;
        if bits as libc::c_int & 2i32 != 0 {
            x[0] = (x[0] as libc::c_int | 0x8000i32) as libc::c_ushort
        }
        bits = ((bits as libc::c_int) << 1i32) as libc::c_ushort;
        x = x.split_at_mut(1).1;
        i += 1
    }
}

pub unsafe extern "C" fn emovz(
    #[slice] mut a: Option<&[libc::c_ushort]>,
    #[slice] mut b: Option<&mut [libc::c_ushort]>,
) {
    let mut i: libc::c_int = 0;
    i = 0i32;
    while i < 10i32 + 3i32 - 1i32 {
        #[slice]
        let fresh26 = a;
        a = Some(&a.unwrap()[1..]);
        #[slice]
        let fresh27;

        {
            let tup = b.unwrap().split_at_mut(1);
            fresh27 = Some(tup.0);

            b = Some(tup.1);
        }

        fresh27.unwrap()[0] = fresh26.unwrap()[0];
        i += 1
    }
    /* clear low guard word */

    b.as_mut().unwrap()[0] = 0i32 as libc::c_ushort;
}

pub unsafe extern "C" fn emovz2(
    #[slice]
    #[nonnull]
    mut a: &[libc::c_ushort],
    #[slice]
    #[nonnull]
    mut b: &mut [libc::c_ushort],
) {
    let mut i: libc::c_int = 0;
    i = 0i32;
    while i < 10i32 + 3i32 - 1i32 {
        #[slice]
        let fresh26 = Some(a);
        a = &a[1..];
        #[slice]
        let fresh27;

        {
            let tup = b.split_at_mut(1);
            fresh27 = Some(tup.0);
            b = tup.1;
        }

        fresh27.unwrap()[0] = fresh26.unwrap()[0];
        i += 1
    }
    /* clear low guard word */
    b[0] = 0i32 as libc::c_ushort;
}

pub unsafe extern "C" fn emovz3(
    #[slice]
    #[nonnull]
    mut a: &[libc::c_ushort],
    #[slice]
    #[nonnull]
    mut b: &mut [libc::c_ushort],
) {
    let mut i: libc::c_int = 0;
    i = 0i32;
    while i < 10i32 + 3i32 - 1i32 {
        #[slice]
        #[nonnull]
        let fresh26 = a;
        a = &a[1..];
        #[nonnull]
        #[slice]
        let fresh27;

        {
            let tup = b.split_at_mut(1);
            fresh27 = tup.0;
            b = tup.1;
        }
        fresh27[0] = fresh26[0];
        i += 1
    }
    /* clear low guard word */
    b[0] = 0i32 as libc::c_ushort;
}

unsafe extern "C" fn eshdn8(#[slice] mut x: Option<&mut [libc::c_ushort]>) {
    let mut newbyt: libc::c_ushort = 0;
    let mut oldbyt: libc::c_ushort = 0;
    let mut i: libc::c_int = 0;
    x = Some(x.unwrap().split_at_mut(2).1);
    oldbyt = 0i32 as libc::c_ushort;
    i = 2i32;
    while i < 10i32 + 3i32 {
        newbyt = ((x.as_mut().unwrap()[0] as libc::c_int) << 8i32) as libc::c_ushort;

        x.as_mut().unwrap()[0] = (x.as_mut().unwrap()[0] as libc::c_int >> 8i32) as libc::c_ushort;

        x.as_mut().unwrap()[0] =
            (x.as_mut().unwrap()[0] as libc::c_int | oldbyt as libc::c_int) as libc::c_ushort;
        oldbyt = newbyt;
        x = Some(x.unwrap().split_at_mut(1).1);
        i += 1
    }
}

pub unsafe extern "C" fn __vsprintf_chk(
    mut buf: Option<Box<libc::c_char>>,
    mut flags: libc::c_int,
    mut slen: size_t,
    mut fmt: Option<&libc::c_char>,
    mut ap: ::std::ffi::VaList,
) -> libc::c_int {
    let mut rv: libc::c_int = 0;
    if slen > 2147483647i32 as size_t {
        rv = vsprintf(
            buf.as_mut()
                .map(|r| &mut **r as *mut _)
                .unwrap_or(0 as *mut _),
            fmt.as_ref()
                .map(|r| &**r as *const _)
                .unwrap_or(0 as *const _),
            ap.as_va_list(),
        )
    } else {
        rv = vsnprintf(
            buf.as_mut()
                .map(|r| &mut **r as *mut _)
                .unwrap_or(0 as *mut _),
            slen,
            fmt.as_ref()
                .map(|r| &**r as *const _)
                .unwrap_or(0 as *const _),
            ap.as_va_list(),
        );
        if rv >= 0i32 && rv as size_t >= slen {
            __chk_fail();
        }
    }
    return rv;
}

unsafe fn non_null_type() {
    let mut ptr = None;
    let mut sptr = None;

    ptr = ::core::ptr::NonNull::new(get_ptr());
    sptr = ::core::ptr::NonNull::new(get_struct_ptr());

    *ptr.unwrap().as_ptr() = 1;
    *ptr.unwrap().as_ptr();
    takes_ptrs(
        ptr.map(|r| r.as_ptr()).unwrap_or(0 as *mut _),
        0 as *const _,
    );

    if *ptr.unwrap().as_ptr() as libc::c_int == ':' as i32
        && *ptr.unwrap().as_ptr().offset(1) as libc::c_int == ':' as i32
    {
        ptr = ::core::ptr::NonNull::new(ptr.unwrap().as_ptr().offset(1))
    }

    (*sptr.unwrap().as_ptr()).data[0] = 1;
}

fn rewritten(#[slice] p: Option<&[u32]>, #[slice] q: Option<&[u32]>) {}

fn array_ref() {
    let mut p: [u32; 4] = [0; 4];
    let mut q: [u32; 4] = [0; 4];

    rewritten(Some(&p), Some(&q));
    rewritten(Some(&p), Some(&q));
}

unsafe extern "C" fn decay_binary(ptr: Option<&_reent>) {
    if ptr
        .as_ref()
        .map(|r| &**r as *const _)
        .unwrap_or(0 as *const _)
        != _impure_ptr
    {}
}

#[ownership_constraints(le(MOVE, _0))]
unsafe extern "C" fn box_to_box(ptr: Option<Box<_reent>>) {
    box_to_box(ptr)
}

#[ownership_constraints(le(WRITE, _0), le(WRITE, _0))]
unsafe extern "C" fn takes_refs(mut _r: Option<&mut _reent>, _r2: Option<&_reent>) {}

#[ownership_constraints(le(MOVE, _0), le(MOVE, _1))]
unsafe extern "C" fn opt_box_to_opt_ref(mut box1: Option<Box<_reent>>, box2: Option<Box<_reent>>) {
    takes_refs(box1.as_mut().map(|r| &mut **r), box2.as_ref().map(|r| &**r));
}

#[ownership_constraints(le(MOVE, _0), le(MOVE, _1))]
unsafe extern "C" fn opt_box_to_ptr(mut box1: Option<Box<u32>>, mut box2: Option<Box<u32>>) {
    takes_ptrs(
        box1.as_mut()
            .map(|r| &mut **r as *mut _)
            .unwrap_or(0 as *mut _),
        box2.as_mut()
            .map(|r| &mut **r as *mut _)
            .unwrap_or(0 as *mut _),
    );
}

unsafe fn array_ref2() {
    let mut q: [u32; 4] = [0; 4];

    let r: [u32; 4] = [0; 4];

    #[nonnull]
    #[slice]
    let mut s: &[u32] = &q;
    #[nonnull]
    #[slice]
    let mut t: &[u32] = &r;

    #[slice]
    #[nonnull]
    let fresh = t;
    t = &t[1..];

    let x = r[1];
}

pub unsafe extern "C" fn deref_to_slice(#[slice] mut s: Option<&[libc::wchar_t]>) {
    deref_to_slice(Some(&s.unwrap()[1..]));
    deref_to_slice(Some(&s.unwrap()[0..]));
}
