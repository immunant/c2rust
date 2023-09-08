#![feature(rustc_private)]

extern crate libc;

extern "C" {
    fn memcpy(_: *mut libc::c_void, _: *const libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    fn memset(_: *mut libc::c_void, _: libc::c_int, _: libc::c_ulong) -> *mut libc::c_void;
}
pub type __uint32_t = libc::c_uint;
pub type uint32_t = __uint32_t;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct MD5_CTX {
    pub state: [uint32_t; 4],
    pub count: [uint32_t; 2],
    pub buffer: [libc::c_uchar; 64],
}
static mut PADDING: [libc::c_uchar; 64] = [
    0x80 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
    0 as libc::c_int as libc::c_uchar,
];

// CHECK-LABEL: MD5_Init
#[no_mangle]
pub unsafe extern "C" fn MD5_Init(mut context: *mut MD5_CTX) {
    (*context).count[1 as libc::c_int as usize] = 0 as libc::c_int as uint32_t;
    (*context).count[0 as libc::c_int as usize] = (*context).count[1 as libc::c_int as usize];
    (*context).state[0 as libc::c_int as usize] = 0x67452301 as libc::c_int as uint32_t;
    (*context).state[1 as libc::c_int as usize] = 0xefcdab89 as libc::c_uint;
    (*context).state[2 as libc::c_int as usize] = 0x98badcfe as libc::c_uint;
    (*context).state[3 as libc::c_int as usize] = 0x10325476 as libc::c_int as uint32_t;
}
#[no_mangle]
pub unsafe extern "C" fn MD5_Update(
    mut context: *mut MD5_CTX,
    mut input: *const libc::c_void,
    mut inputLen: libc::c_uint,
) {
    let mut i: libc::c_uint = 0;
    let mut ndx: libc::c_uint = 0;
    let mut partLen: libc::c_uint = 0;
    // let mut input: *const libc::c_uchar = _input as *const libc::c_uchar;
    ndx = (*context).count[0 as libc::c_int as usize] >> 3 as libc::c_int
        & 0x3f as libc::c_int as libc::c_uint;
    (*context).count[0 as libc::c_int as usize] =
        ((*context).count[0 as libc::c_int as usize] as libc::c_uint)
            .wrapping_add(inputLen << 3 as libc::c_int) as uint32_t as uint32_t;
    if (*context).count[0 as libc::c_int as usize] < inputLen << 3 as libc::c_int {
        (*context).count[1 as libc::c_int as usize] =
            ((*context).count[1 as libc::c_int as usize]).wrapping_add(1);
    }
    (*context).count[1 as libc::c_int as usize] =
        ((*context).count[1 as libc::c_int as usize] as libc::c_uint)
            .wrapping_add(inputLen >> 29 as libc::c_int) as uint32_t as uint32_t;
    partLen = (64 as libc::c_int as libc::c_uint).wrapping_sub(ndx);
    if inputLen >= partLen {
        memcpy(
            &mut *((*context).buffer).as_mut_ptr().offset(ndx as isize) as *mut libc::c_uchar
                as *mut libc::c_void,
            input /* as *mut libc::c_uchar */ as *const libc::c_void,
            partLen as libc::c_ulong,
        );
        li_MD5Transform(
            ((*context).state).as_mut_ptr(),
            ((*context).buffer).as_mut_ptr() as *const libc::c_uchar,
        );
        i = partLen;
        while i.wrapping_add(63 as libc::c_int as libc::c_uint) < inputLen {
            // li_MD5Transform(((*context).state).as_mut_ptr(), &*input.offset(i as isize));
            i = i.wrapping_add(64 as libc::c_int as libc::c_uint);
        }
        ndx = 0 as libc::c_int as libc::c_uint;
    } else {
        i = 0 as libc::c_int as libc::c_uint;
    }
    // memcpy(
    //     &mut *((*context).buffer).as_mut_ptr().offset(ndx as isize) as *mut libc::c_uchar
    //         as *mut libc::c_void,
    //     &*input.offset(i as isize) as *const libc::c_uchar as *mut libc::c_uchar
    //         as *const libc::c_void,
    //     inputLen.wrapping_sub(i) as libc::c_ulong,
    // );
}
#[no_mangle]
pub unsafe extern "C" fn MD5_Final(mut digest: *mut libc::c_uchar, mut context: *mut MD5_CTX) {
    let mut bits: [libc::c_uchar; 8] = [0; 8];
    let mut ndx: libc::c_uint = 0;
    let mut padLen: libc::c_uint = 0;
    // Encode(
    //     bits.as_mut_ptr(),
    //     ((*context).count).as_mut_ptr(),
    //     8 as libc::c_int as libc::c_uint,
    // );
    ndx = (*context).count[0 as libc::c_int as usize] >> 3 as libc::c_int
        & 0x3f as libc::c_int as libc::c_uint;
    padLen = if ndx < 56 as libc::c_int as libc::c_uint {
        (56 as libc::c_int as libc::c_uint).wrapping_sub(ndx)
    } else {
        (120 as libc::c_int as libc::c_uint).wrapping_sub(ndx)
    };
    // MD5_Update(context, PADDING.as_mut_ptr(),// as *const libc::c_void,
    //     padLen);
    // MD5_Update(
    //     context,
    //     bits.as_mut_ptr(), // as *const libc::c_void,
    //     8 as libc::c_int as libc::c_uint,
    // );
    // Encode(
    //     digest,
    //     ((*context).state).as_mut_ptr(),
    //     16 as libc::c_int as libc::c_uint,
    // );
    memset(
        context /* as *mut libc::c_uchar */ as *mut libc::c_void,
        0 as libc::c_int,
        ::std::mem::size_of::<MD5_CTX>() as libc::c_ulong,
    );
}
unsafe extern "C" fn li_MD5Transform(mut state: *mut uint32_t, mut block: *const libc::c_uchar) {
    let mut a: uint32_t = *state.offset(0 as libc::c_int as isize);
    let mut b: uint32_t = *state.offset(1 as libc::c_int as isize);
    let mut c: uint32_t = *state.offset(2 as libc::c_int as isize);
    let mut d: uint32_t = *state.offset(3 as libc::c_int as isize);
    let mut x: [uint32_t; 16] = [0; 16];
    // Decode(x.as_mut_ptr(), block, 64 as libc::c_int as libc::c_uint);
    a = (a as libc::c_uint).wrapping_add(
        (b & c | !b & d)
            .wrapping_add(x[0 as libc::c_int as usize])
            .wrapping_add(0xd76aa478 as libc::c_uint),
    ) as uint32_t as uint32_t;
    a = a << 7 as libc::c_int | a >> 32 as libc::c_int - 7 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a & b | !a & c)
            .wrapping_add(x[1 as libc::c_int as usize])
            .wrapping_add(0xe8c7b756 as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 12 as libc::c_int | d >> 32 as libc::c_int - 12 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d & a | !d & b)
            .wrapping_add(x[2 as libc::c_int as usize])
            .wrapping_add(0x242070db as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    c = c << 17 as libc::c_int | c >> 32 as libc::c_int - 17 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c & d | !c & a)
            .wrapping_add(x[3 as libc::c_int as usize])
            .wrapping_add(0xc1bdceee as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 22 as libc::c_int | b >> 32 as libc::c_int - 22 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (b & c | !b & d)
            .wrapping_add(x[4 as libc::c_int as usize])
            .wrapping_add(0xf57c0faf as libc::c_uint),
    ) as uint32_t as uint32_t;
    a = a << 7 as libc::c_int | a >> 32 as libc::c_int - 7 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a & b | !a & c)
            .wrapping_add(x[5 as libc::c_int as usize])
            .wrapping_add(0x4787c62a as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    d = d << 12 as libc::c_int | d >> 32 as libc::c_int - 12 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d & a | !d & b)
            .wrapping_add(x[6 as libc::c_int as usize])
            .wrapping_add(0xa8304613 as libc::c_uint),
    ) as uint32_t as uint32_t;
    c = c << 17 as libc::c_int | c >> 32 as libc::c_int - 17 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c & d | !c & a)
            .wrapping_add(x[7 as libc::c_int as usize])
            .wrapping_add(0xfd469501 as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 22 as libc::c_int | b >> 32 as libc::c_int - 22 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (b & c | !b & d)
            .wrapping_add(x[8 as libc::c_int as usize])
            .wrapping_add(0x698098d8 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    a = a << 7 as libc::c_int | a >> 32 as libc::c_int - 7 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a & b | !a & c)
            .wrapping_add(x[9 as libc::c_int as usize])
            .wrapping_add(0x8b44f7af as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 12 as libc::c_int | d >> 32 as libc::c_int - 12 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d & a | !d & b)
            .wrapping_add(x[10 as libc::c_int as usize])
            .wrapping_add(0xffff5bb1 as libc::c_uint),
    ) as uint32_t as uint32_t;
    c = c << 17 as libc::c_int | c >> 32 as libc::c_int - 17 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c & d | !c & a)
            .wrapping_add(x[11 as libc::c_int as usize])
            .wrapping_add(0x895cd7be as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 22 as libc::c_int | b >> 32 as libc::c_int - 22 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (b & c | !b & d)
            .wrapping_add(x[12 as libc::c_int as usize])
            .wrapping_add(0x6b901122 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    a = a << 7 as libc::c_int | a >> 32 as libc::c_int - 7 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a & b | !a & c)
            .wrapping_add(x[13 as libc::c_int as usize])
            .wrapping_add(0xfd987193 as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 12 as libc::c_int | d >> 32 as libc::c_int - 12 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d & a | !d & b)
            .wrapping_add(x[14 as libc::c_int as usize])
            .wrapping_add(0xa679438e as libc::c_uint),
    ) as uint32_t as uint32_t;
    c = c << 17 as libc::c_int | c >> 32 as libc::c_int - 17 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c & d | !c & a)
            .wrapping_add(x[15 as libc::c_int as usize])
            .wrapping_add(0x49b40821 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    b = b << 22 as libc::c_int | b >> 32 as libc::c_int - 22 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (b & d | c & !d)
            .wrapping_add(x[1 as libc::c_int as usize])
            .wrapping_add(0xf61e2562 as libc::c_uint),
    ) as uint32_t as uint32_t;
    a = a << 5 as libc::c_int | a >> 32 as libc::c_int - 5 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a & c | b & !c)
            .wrapping_add(x[6 as libc::c_int as usize])
            .wrapping_add(0xc040b340 as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 9 as libc::c_int | d >> 32 as libc::c_int - 9 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d & b | a & !b)
            .wrapping_add(x[11 as libc::c_int as usize])
            .wrapping_add(0x265e5a51 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    c = c << 14 as libc::c_int | c >> 32 as libc::c_int - 14 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c & a | d & !a)
            .wrapping_add(x[0 as libc::c_int as usize])
            .wrapping_add(0xe9b6c7aa as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 20 as libc::c_int | b >> 32 as libc::c_int - 20 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (b & d | c & !d)
            .wrapping_add(x[5 as libc::c_int as usize])
            .wrapping_add(0xd62f105d as libc::c_uint),
    ) as uint32_t as uint32_t;
    a = a << 5 as libc::c_int | a >> 32 as libc::c_int - 5 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a & c | b & !c)
            .wrapping_add(x[10 as libc::c_int as usize])
            .wrapping_add(0x2441453 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    d = d << 9 as libc::c_int | d >> 32 as libc::c_int - 9 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d & b | a & !b)
            .wrapping_add(x[15 as libc::c_int as usize])
            .wrapping_add(0xd8a1e681 as libc::c_uint),
    ) as uint32_t as uint32_t;
    c = c << 14 as libc::c_int | c >> 32 as libc::c_int - 14 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c & a | d & !a)
            .wrapping_add(x[4 as libc::c_int as usize])
            .wrapping_add(0xe7d3fbc8 as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 20 as libc::c_int | b >> 32 as libc::c_int - 20 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (b & d | c & !d)
            .wrapping_add(x[9 as libc::c_int as usize])
            .wrapping_add(0x21e1cde6 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    a = a << 5 as libc::c_int | a >> 32 as libc::c_int - 5 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a & c | b & !c)
            .wrapping_add(x[14 as libc::c_int as usize])
            .wrapping_add(0xc33707d6 as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 9 as libc::c_int | d >> 32 as libc::c_int - 9 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d & b | a & !b)
            .wrapping_add(x[3 as libc::c_int as usize])
            .wrapping_add(0xf4d50d87 as libc::c_uint),
    ) as uint32_t as uint32_t;
    c = c << 14 as libc::c_int | c >> 32 as libc::c_int - 14 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c & a | d & !a)
            .wrapping_add(x[8 as libc::c_int as usize])
            .wrapping_add(0x455a14ed as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    b = b << 20 as libc::c_int | b >> 32 as libc::c_int - 20 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (b & d | c & !d)
            .wrapping_add(x[13 as libc::c_int as usize])
            .wrapping_add(0xa9e3e905 as libc::c_uint),
    ) as uint32_t as uint32_t;
    a = a << 5 as libc::c_int | a >> 32 as libc::c_int - 5 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a & c | b & !c)
            .wrapping_add(x[2 as libc::c_int as usize])
            .wrapping_add(0xfcefa3f8 as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 9 as libc::c_int | d >> 32 as libc::c_int - 9 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d & b | a & !b)
            .wrapping_add(x[7 as libc::c_int as usize])
            .wrapping_add(0x676f02d9 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    c = c << 14 as libc::c_int | c >> 32 as libc::c_int - 14 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c & a | d & !a)
            .wrapping_add(x[12 as libc::c_int as usize])
            .wrapping_add(0x8d2a4c8a as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 20 as libc::c_int | b >> 32 as libc::c_int - 20 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (b ^ c ^ d)
            .wrapping_add(x[5 as libc::c_int as usize])
            .wrapping_add(0xfffa3942 as libc::c_uint),
    ) as uint32_t as uint32_t;
    a = a << 4 as libc::c_int | a >> 32 as libc::c_int - 4 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a ^ b ^ c)
            .wrapping_add(x[8 as libc::c_int as usize])
            .wrapping_add(0x8771f681 as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 11 as libc::c_int | d >> 32 as libc::c_int - 11 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d ^ a ^ b)
            .wrapping_add(x[11 as libc::c_int as usize])
            .wrapping_add(0x6d9d6122 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    c = c << 16 as libc::c_int | c >> 32 as libc::c_int - 16 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c ^ d ^ a)
            .wrapping_add(x[14 as libc::c_int as usize])
            .wrapping_add(0xfde5380c as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 23 as libc::c_int | b >> 32 as libc::c_int - 23 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (b ^ c ^ d)
            .wrapping_add(x[1 as libc::c_int as usize])
            .wrapping_add(0xa4beea44 as libc::c_uint),
    ) as uint32_t as uint32_t;
    a = a << 4 as libc::c_int | a >> 32 as libc::c_int - 4 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a ^ b ^ c)
            .wrapping_add(x[4 as libc::c_int as usize])
            .wrapping_add(0x4bdecfa9 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    d = d << 11 as libc::c_int | d >> 32 as libc::c_int - 11 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d ^ a ^ b)
            .wrapping_add(x[7 as libc::c_int as usize])
            .wrapping_add(0xf6bb4b60 as libc::c_uint),
    ) as uint32_t as uint32_t;
    c = c << 16 as libc::c_int | c >> 32 as libc::c_int - 16 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c ^ d ^ a)
            .wrapping_add(x[10 as libc::c_int as usize])
            .wrapping_add(0xbebfbc70 as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 23 as libc::c_int | b >> 32 as libc::c_int - 23 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (b ^ c ^ d)
            .wrapping_add(x[13 as libc::c_int as usize])
            .wrapping_add(0x289b7ec6 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    a = a << 4 as libc::c_int | a >> 32 as libc::c_int - 4 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a ^ b ^ c)
            .wrapping_add(x[0 as libc::c_int as usize])
            .wrapping_add(0xeaa127fa as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 11 as libc::c_int | d >> 32 as libc::c_int - 11 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d ^ a ^ b)
            .wrapping_add(x[3 as libc::c_int as usize])
            .wrapping_add(0xd4ef3085 as libc::c_uint),
    ) as uint32_t as uint32_t;
    c = c << 16 as libc::c_int | c >> 32 as libc::c_int - 16 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c ^ d ^ a)
            .wrapping_add(x[6 as libc::c_int as usize])
            .wrapping_add(0x4881d05 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    b = b << 23 as libc::c_int | b >> 32 as libc::c_int - 23 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (b ^ c ^ d)
            .wrapping_add(x[9 as libc::c_int as usize])
            .wrapping_add(0xd9d4d039 as libc::c_uint),
    ) as uint32_t as uint32_t;
    a = a << 4 as libc::c_int | a >> 32 as libc::c_int - 4 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (a ^ b ^ c)
            .wrapping_add(x[12 as libc::c_int as usize])
            .wrapping_add(0xe6db99e5 as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 11 as libc::c_int | d >> 32 as libc::c_int - 11 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (d ^ a ^ b)
            .wrapping_add(x[15 as libc::c_int as usize])
            .wrapping_add(0x1fa27cf8 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    c = c << 16 as libc::c_int | c >> 32 as libc::c_int - 16 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (c ^ d ^ a)
            .wrapping_add(x[2 as libc::c_int as usize])
            .wrapping_add(0xc4ac5665 as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 23 as libc::c_int | b >> 32 as libc::c_int - 23 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (c ^ (b | !d))
            .wrapping_add(x[0 as libc::c_int as usize])
            .wrapping_add(0xf4292244 as libc::c_uint),
    ) as uint32_t as uint32_t;
    a = a << 6 as libc::c_int | a >> 32 as libc::c_int - 6 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (b ^ (a | !c))
            .wrapping_add(x[7 as libc::c_int as usize])
            .wrapping_add(0x432aff97 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    d = d << 10 as libc::c_int | d >> 32 as libc::c_int - 10 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (a ^ (d | !b))
            .wrapping_add(x[14 as libc::c_int as usize])
            .wrapping_add(0xab9423a7 as libc::c_uint),
    ) as uint32_t as uint32_t;
    c = c << 15 as libc::c_int | c >> 32 as libc::c_int - 15 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (d ^ (c | !a))
            .wrapping_add(x[5 as libc::c_int as usize])
            .wrapping_add(0xfc93a039 as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 21 as libc::c_int | b >> 32 as libc::c_int - 21 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (c ^ (b | !d))
            .wrapping_add(x[12 as libc::c_int as usize])
            .wrapping_add(0x655b59c3 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    a = a << 6 as libc::c_int | a >> 32 as libc::c_int - 6 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (b ^ (a | !c))
            .wrapping_add(x[3 as libc::c_int as usize])
            .wrapping_add(0x8f0ccc92 as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 10 as libc::c_int | d >> 32 as libc::c_int - 10 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (a ^ (d | !b))
            .wrapping_add(x[10 as libc::c_int as usize])
            .wrapping_add(0xffeff47d as libc::c_uint),
    ) as uint32_t as uint32_t;
    c = c << 15 as libc::c_int | c >> 32 as libc::c_int - 15 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (d ^ (c | !a))
            .wrapping_add(x[1 as libc::c_int as usize])
            .wrapping_add(0x85845dd1 as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 21 as libc::c_int | b >> 32 as libc::c_int - 21 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (c ^ (b | !d))
            .wrapping_add(x[8 as libc::c_int as usize])
            .wrapping_add(0x6fa87e4f as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    a = a << 6 as libc::c_int | a >> 32 as libc::c_int - 6 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (b ^ (a | !c))
            .wrapping_add(x[15 as libc::c_int as usize])
            .wrapping_add(0xfe2ce6e0 as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 10 as libc::c_int | d >> 32 as libc::c_int - 10 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (a ^ (d | !b))
            .wrapping_add(x[6 as libc::c_int as usize])
            .wrapping_add(0xa3014314 as libc::c_uint),
    ) as uint32_t as uint32_t;
    c = c << 15 as libc::c_int | c >> 32 as libc::c_int - 15 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (d ^ (c | !a))
            .wrapping_add(x[13 as libc::c_int as usize])
            .wrapping_add(0x4e0811a1 as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    b = b << 21 as libc::c_int | b >> 32 as libc::c_int - 21 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    a = (a as libc::c_uint).wrapping_add(
        (c ^ (b | !d))
            .wrapping_add(x[4 as libc::c_int as usize])
            .wrapping_add(0xf7537e82 as libc::c_uint),
    ) as uint32_t as uint32_t;
    a = a << 6 as libc::c_int | a >> 32 as libc::c_int - 6 as libc::c_int;
    a = (a as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    d = (d as libc::c_uint).wrapping_add(
        (b ^ (a | !c))
            .wrapping_add(x[11 as libc::c_int as usize])
            .wrapping_add(0xbd3af235 as libc::c_uint),
    ) as uint32_t as uint32_t;
    d = d << 10 as libc::c_int | d >> 32 as libc::c_int - 10 as libc::c_int;
    d = (d as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    c = (c as libc::c_uint).wrapping_add(
        (a ^ (d | !b))
            .wrapping_add(x[2 as libc::c_int as usize])
            .wrapping_add(0x2ad7d2bb as libc::c_int as uint32_t),
    ) as uint32_t as uint32_t;
    c = c << 15 as libc::c_int | c >> 32 as libc::c_int - 15 as libc::c_int;
    c = (c as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    b = (b as libc::c_uint).wrapping_add(
        (d ^ (c | !a))
            .wrapping_add(x[9 as libc::c_int as usize])
            .wrapping_add(0xeb86d391 as libc::c_uint),
    ) as uint32_t as uint32_t;
    b = b << 21 as libc::c_int | b >> 32 as libc::c_int - 21 as libc::c_int;
    b = (b as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    let ref mut fresh0 = *state.offset(0 as libc::c_int as isize);
    *fresh0 = (*fresh0 as libc::c_uint).wrapping_add(a) as uint32_t as uint32_t;
    let ref mut fresh1 = *state.offset(1 as libc::c_int as isize);
    *fresh1 = (*fresh1 as libc::c_uint).wrapping_add(b) as uint32_t as uint32_t;
    let ref mut fresh2 = *state.offset(2 as libc::c_int as isize);
    *fresh2 = (*fresh2 as libc::c_uint).wrapping_add(c) as uint32_t as uint32_t;
    let ref mut fresh3 = *state.offset(3 as libc::c_int as isize);
    *fresh3 = (*fresh3 as libc::c_uint).wrapping_add(d) as uint32_t as uint32_t;
    memset(
        x.as_mut_ptr() /* as *mut libc::c_uchar */ as *mut libc::c_void,
        0 as libc::c_int,
        ::std::mem::size_of::<[uint32_t; 16]>() as libc::c_ulong,
    );
}
unsafe extern "C" fn Encode(
    mut output: *mut libc::c_uchar,
    mut input: *mut uint32_t,
    mut len: libc::c_uint,
) {
    let mut i: libc::c_uint = 0;
    let mut j: libc::c_uint = 0;
    i = 0 as libc::c_int as libc::c_uint;
    j = 0 as libc::c_int as libc::c_uint;
    while j < len {
        *output.offset(j as isize) =
            (*input.offset(i as isize) & 0xff as libc::c_int as libc::c_uint) as libc::c_uchar;
        *output.offset(j.wrapping_add(1 as libc::c_int as libc::c_uint) as isize) =
            (*input.offset(i as isize) >> 8 as libc::c_int & 0xff as libc::c_int as libc::c_uint)
                as libc::c_uchar;
        *output.offset(j.wrapping_add(2 as libc::c_int as libc::c_uint) as isize) =
            (*input.offset(i as isize) >> 16 as libc::c_int & 0xff as libc::c_int as libc::c_uint)
                as libc::c_uchar;
        *output.offset(j.wrapping_add(3 as libc::c_int as libc::c_uint) as isize) =
            (*input.offset(i as isize) >> 24 as libc::c_int & 0xff as libc::c_int as libc::c_uint)
                as libc::c_uchar;
        i = i.wrapping_add(1);
        j = j.wrapping_add(4 as libc::c_int as libc::c_uint);
    }
}
unsafe extern "C" fn Decode(
    mut output: *mut uint32_t,
    mut input: *const libc::c_uchar,
    mut len: libc::c_uint,
) {
    let mut i: libc::c_uint = 0;
    let mut j: libc::c_uint = 0;
    i = 0 as libc::c_int as libc::c_uint;
    j = 0 as libc::c_int as libc::c_uint;
    while j < len {
        *output.offset(i as isize) = *input.offset(j as isize) as uint32_t
            | (*input.offset(j.wrapping_add(1 as libc::c_int as libc::c_uint) as isize)
                as uint32_t)
                << 8 as libc::c_int
            | (*input.offset(j.wrapping_add(2 as libc::c_int as libc::c_uint) as isize)
                as uint32_t)
                << 16 as libc::c_int
            | (*input.offset(j.wrapping_add(3 as libc::c_int as libc::c_uint) as isize)
                as uint32_t)
                << 24 as libc::c_int;
        i = i.wrapping_add(1);
        j = j.wrapping_add(4 as libc::c_int as libc::c_uint);
    }
}
