//! --use-manual-shims
#![feature(rustc_private)]
#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]

/*
This file should compile as 100% safe code after running `c2rust-analyze` and appropriate cleanup.
```sh
# Run `c2rust-analyze` with standard options
cargo test lighttpd_buffer

# From `lighttpd_buffer.rs.analysis.txt`, manually copy the lines between the `==== BEGIN` and
# `==== END` markers into a separate file, `test_buffer.rs`.

# Auto-fix remaining compile errors
rustc --crate-type rlib --error-format json test_buffer.rs 2>errors.json
./scripts/auto_fix_errors.py errors.json
# Run again to fix newly uncovered errors
rustc --crate-type rlib --error-format json test_buffer.rs 2>errors.json
./scripts/auto_fix_errors.py errors.json

# Remove `unsafe`
sed -i -e 's/\<unsafe //' test_buffer.rs

# Code should now compile without errors
rustc --crate-type rlib test_buffer.rs -A warnings
```
*/

extern crate libc;
extern "C" {
//    fn ck_assert_failed(
//        filename: *const libc::c_char,
//        line: libc::c_uint,
//        msg: *const libc::c_char,
//    ) -> !;
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn memcpy(
        _: *mut libc::c_void,
        _: *const libc::c_void,
        _: libc::c_ulong,
    ) -> *mut libc::c_void;
    fn memmove(
        _: *mut libc::c_void,
        _: *const libc::c_void,
        _: libc::c_ulong,
    ) -> *mut libc::c_void;
    fn memcmp(
        _: *const libc::c_void,
        _: *const libc::c_void,
        _: libc::c_ulong,
    ) -> libc::c_int;
    fn memchr(
        _: *const libc::c_void,
        _: libc::c_int,
        _: libc::c_ulong,
    ) -> *mut libc::c_void;
    fn strlen(_: *const libc::c_char) -> libc::c_ulong;
    fn mempcpy(
        _: *mut libc::c_void,
        _: *const libc::c_void,
        _: libc::c_ulong,
    ) -> *mut libc::c_void;
    /*
    fn strftime(
        __s: *mut libc::c_char,
        __maxsize: size_t,
        __format: *const libc::c_char,
        __tp: *const tm,
    ) -> size_t;
    */
}

#[no_mangle]
#[cold]
#[c2rust_analyze_test::fixed_signature]
pub unsafe extern "C" fn ck_assert_failed(
    /* TODO: all ck_assert_failed arguments are commented out due to bad casts at call sites
    mut filename: *const libc::c_char,
    mut line: libc::c_uint,
    mut msg: *const libc::c_char,
    */
) -> ! {
    panic!();
    /*
    ck_bt_stderr(
        filename,
        line,
        msg,
        b"%s.%u: assertion failed: %s\n\0" as *const u8 as *const libc::c_char,
    );
    abort();
    */
}

pub type __uint32_t = libc::c_uint;
pub type __intmax_t = libc::c_long;
pub type __uintmax_t = libc::c_ulong;
pub type size_t = libc::c_ulong;
pub type uint32_t = __uint32_t;
pub type uint_fast32_t = libc::c_ulong;
pub type intmax_t = __intmax_t;
pub type uintmax_t = __uintmax_t;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tm {
    pub tm_sec: libc::c_int,
    pub tm_min: libc::c_int,
    pub tm_hour: libc::c_int,
    pub tm_mday: libc::c_int,
    pub tm_mon: libc::c_int,
    pub tm_year: libc::c_int,
    pub tm_wday: libc::c_int,
    pub tm_yday: libc::c_int,
    pub tm_isdst: libc::c_int,
    pub tm_gmtoff: libc::c_long,
    pub tm_zone: *const libc::c_char,
}
//#[derive(Copy, Clone)]
#[derive(Clone)]
#[repr(C)]
pub struct buffer {
    pub ptr: *mut libc::c_char,
    pub used: uint32_t,
    pub size: uint32_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct const_iovec {
    pub iov_base: *const libc::c_void,
    pub iov_len: size_t,
}
pub type buffer_encoding_t = libc::c_uint;
pub const ENCODING_MINIMAL_XML: buffer_encoding_t = 3;
pub const ENCODING_HTML: buffer_encoding_t = 2;
pub const ENCODING_REL_URI_PART: buffer_encoding_t = 1;
pub const ENCODING_REL_URI: buffer_encoding_t = 0;
// CHECK-LABEL: fn buffer_is_blank<'h0>(mut b: &'h0 (buffer)) -> libc::c_int {
#[inline]
unsafe extern "C" fn buffer_is_blank(mut b: *const buffer) -> libc::c_int {
    return ((*b).used < 2 as libc::c_int as libc::c_uint) as libc::c_int;
}
// CHECK-LABEL: fn buffer_clen<'h0>(mut b: &'h0 (buffer)) -> uint32_t {
#[inline]
unsafe extern "C" fn buffer_clen(mut b: *const buffer) -> uint32_t {
    return ((*b).used)
        .wrapping_sub(
            (0 as libc::c_int as libc::c_uint != (*b).used) as libc::c_int
                as libc::c_uint,
        );
}
// CHECK-LABEL: fn buffer_truncate<'h0>(mut b: &'h0 mut (buffer), mut len: uint32_t) {
#[inline]
unsafe extern "C" fn buffer_truncate(mut b: *mut buffer, mut len: uint32_t) {
    // CHECK: let (arr, idx, ) =
    // CHECK: &mut arr[idx ..]
    *((*b).ptr).offset(len as isize) = '\u{0}' as i32 as libc::c_char;
    (*b).used = len.wrapping_add(1 as libc::c_int as libc::c_uint);
}
// CHECK-LABEL: fn buffer_blank<'h0>(mut b: &'h0 mut (buffer)) {
#[inline]
unsafe extern "C" fn buffer_blank(mut b: *mut buffer) {
    // CHECK: if {{.*}}.is_none()
    if !((*b).ptr).is_null() {
        // CHECK: buffer_truncate
        buffer_truncate(b, 0 as libc::c_int as uint32_t);
    } else {
        // CHECK: buffer_extend
        buffer_extend(b, 0 as libc::c_int as size_t);
    };
}
// CHECK-LABEL: fn buffer_clear<'h0>(mut b: &'h0 mut (buffer)) {
#[inline]
unsafe extern "C" fn buffer_clear(mut b: *mut buffer) {
    (*b).used = 0 as libc::c_int as uint32_t;
}
/* TODO: needs [u8] -> [i8] safe cast helper
static mut hex_chars_lc: [libc::c_char; 17] = unsafe {
    *::std::mem::transmute::<&[u8; 17], &[libc::c_char; 17]>(b"0123456789abcdef\0")
};
static mut hex_chars_uc: [libc::c_char; 17] = unsafe {
    *::std::mem::transmute::<&[u8; 17], &[libc::c_char; 17]>(b"0123456789ABCDEF\0")
};
*/
// CHECK-LABEL: fn buffer_init<'h0>() -> {{.*}}Result<{{.*}}Box<(buffer)>,()> {
#[no_mangle]
#[inline(never)]
pub unsafe extern "C" fn buffer_init() -> *mut buffer {
    // CHECK: let b: {{.*}}Result<{{.*}}Box<(buffer)>,()>
    // CHECK: Box::new(buffer {
    let b: *mut buffer = calloc(
        1 as libc::c_int as libc::c_ulong,
        ::std::mem::size_of::<buffer>() as libc::c_ulong,
    ) as *mut buffer;
    // `b` is inferred to be non-nullable, so `b.is_null()` gets rewritten to constant `false`.
    // CHECK: if false {
    if b.is_null() {
        ck_assert_failed(
            /*
            b"src/buffer.c\0" as *const u8 as *const libc::c_char,
            16 as libc::c_int as libc::c_uint,
            b"b\0" as *const u8 as *const libc::c_char,
            */
        );
    }
    return b;
}
// CHECK-LABEL: fn buffer_free<'h0>(mut b: {{.*}}Option<{{.*}}Result<{{.*}}Box<(buffer)>,()>>) {
#[no_mangle]
pub unsafe extern "C" fn buffer_free(mut b: *mut buffer) {
    // CHECK: .is_none()
    if b.is_null() {
        return;
    }
    // The first `free` takes (dynamic) ownership of `b.ptr`.  The second takes ownership of `b`.
    // CHECK: drop({{.*}}mem::replace(
    free((*b).ptr as *mut libc::c_void);
    // CHECK: drop({{.*}}mem::replace(
    free(b as *mut libc::c_void);
}
// CHECK-LABEL: fn buffer_free_ptr<'h0>(mut b: &'h0 mut (buffer)) {
#[no_mangle]
#[cold]
pub unsafe extern "C" fn buffer_free_ptr(mut b: *mut buffer) {
    // CHECK: drop({{.*}}mem::replace(
    free((*b).ptr as *mut libc::c_void);
    // CHECK: (*b).ptr = None;
    (*b).ptr = 0 as *mut libc::c_char;
    (*b).used = 0 as libc::c_int as uint32_t;
    (*b).size = 0 as libc::c_int as uint32_t;
}
// CHECK-LABEL: fn buffer_move<'h0,'h1>(mut b: &'h0 mut (buffer), mut src: &'h1 mut (buffer)) {
#[no_mangle]
pub unsafe extern "C" fn buffer_move(mut b: *mut buffer, mut src: *mut buffer) {
    let mut tmp: buffer = buffer {
        ptr: 0 as *mut libc::c_char,
        used: 0,
        size: 0,
    };
    buffer_clear(b);
    //tmp = *src;
    //*src = *b;
    //*b = tmp;
    tmp.ptr = (*src).ptr;
    tmp.used = (*src).used;
    tmp.size = (*src).size;
    (*src).ptr = (*b).ptr;
    (*src).used = (*b).used;
    (*src).size = (*b).size;
    (*b).ptr = tmp.ptr;
    (*b).used = tmp.used;
    (*b).size = tmp.size;
}
// CHECK-LABEL: fn buffer_realloc<'h0,'h1>(b: {{.*}}Option<&'h0 mut (buffer)>, len: size_t) -> {{.*}}Option<&'h1 mut [(libc::c_char)]> {
#[cold]
#[inline(never)]
unsafe extern "C" fn buffer_realloc(b: *mut buffer, len: size_t) -> *mut libc::c_char {
    let mut sz: size_t = len
        .wrapping_add(1 as libc::c_int as libc::c_ulong)
        .wrapping_add(64 as libc::c_ulong)
        .wrapping_sub(1 as libc::c_int as libc::c_ulong)
        & !(64 as libc::c_ulong).wrapping_sub(1 as libc::c_int as libc::c_ulong);
    if !(sz > len) {
        ck_assert_failed(
            /*
            b"src/buffer.c\0" as *const u8 as *const libc::c_char,
            47 as libc::c_int as libc::c_uint,
            b"sz > len\0" as *const u8 as *const libc::c_char,
            */
        );
    }
    if sz & sz.wrapping_sub(1 as libc::c_int as libc::c_ulong) != 0
        && sz < 2147483647 as libc::c_int as libc::c_ulong
    {
        let psz: size_t = sz;
        sz = 256 as libc::c_int as size_t;
        while sz < psz {
            sz <<= 1 as libc::c_int;
        }
    }
    sz |= 1 as libc::c_int as libc::c_ulong;
    (*b).size = sz as uint32_t;
    // CHECK: (b){{.*}}.ptr = {{.*}}Option::Some({{.*}}Result{{.*}}Ok(
    // CHECK: let (src_ptr, dest_byte_len, ) =
    // CHECK: dest_ptr.resize_with(
    // CHECK: dest_ptr.into_boxed_slice()
    (*b).ptr = realloc((*b).ptr as *mut libc::c_void, sz) as *mut libc::c_char;
    // Even though the translation of `realloc` unconditionally returns `Some`, the result is
    // stored in `(*b).ptr`, which is nullable, so this null check will remain in place.
    // CHECK: if {{.*}}.is_none() {
    if ((*b).ptr).is_null() {
        ck_assert_failed(
            /*
            b"src/buffer.c\0" as *const u8 as *const libc::c_char,
            58 as libc::c_int as libc::c_uint,
            b"((void*)0) != b->ptr\0" as *const u8 as *const libc::c_char,
            */
        );
    }
    return (*b).ptr;
}
// CHECK-LABEL: fn buffer_alloc_replace<'h0,'h1>(
#[cold]
#[inline(never)]
unsafe extern "C" fn buffer_alloc_replace(
    // CHECK: b: &'h0 mut (buffer),
    b: *mut buffer,
    // CHECK: size: size_t,
    size: size_t,
// CHECK: ) -> {{.*}}Option<&'h1 mut [(libc::c_char)]> {
) -> *mut libc::c_char {
    if !((*b).ptr).is_null() {
        free((*b).ptr as *mut libc::c_void);
        (*b).ptr = 0 as *mut libc::c_char;
    }
    let bsize2x: size_t = ((*b).size as libc::c_ulong & !(1 as libc::c_ulong))
        << 1 as libc::c_int;
    return buffer_realloc(
        b,
        if bsize2x > size {
            bsize2x.wrapping_sub(1 as libc::c_int as libc::c_ulong)
        } else {
            size
        },
    );
}
// CHECK-LABEL: fn buffer_string_prepare_copy<'h0,'h1>(
#[no_mangle]
pub unsafe extern "C" fn buffer_string_prepare_copy(
    // CHECK: b: &'h0 mut (buffer),
    b: *mut buffer,
    // CHECK: size: size_t,
    size: size_t,
// CHECK: ) -> {{.*}}Option<&'h1 mut [(libc::c_char)]> {
) -> *mut libc::c_char {
    (*b).used = 0 as libc::c_int as uint32_t;
    return if size < (*b).size as libc::c_ulong {
        (*b).ptr
    } else {
        buffer_alloc_replace(b, size)
    };
}
// CHECK-LABEL: fn buffer_string_prepare_append_resize<'h0,'h1>(
#[cold]
#[inline(never)]
unsafe extern "C" fn buffer_string_prepare_append_resize(
    // CHECK: b: &'h0 mut (buffer),
    b: *mut buffer,
    // CHECK: size: size_t,
    size: size_t,
// CHECK: ) -> {{.*}}Option<&'h1 mut [(libc::c_char)]> {
) -> *mut libc::c_char {
    if (*b).used < 2 as libc::c_int as libc::c_uint {
        let s: *mut libc::c_char = buffer_string_prepare_copy(b, size);
        *s = '\u{0}' as i32 as libc::c_char;
        return s;
    }
    let bsize2x: size_t = ((*b).size as libc::c_ulong & !(1 as libc::c_ulong))
        << 1 as libc::c_int;
    let req_size: size_t = if bsize2x.wrapping_sub((*b).used as libc::c_ulong) > size {
        bsize2x.wrapping_sub(1 as libc::c_int as libc::c_ulong)
    } else {
        ((*b).used as libc::c_ulong).wrapping_add(size)
    };
    if !(req_size >= (*b).used as libc::c_ulong) {
        ck_assert_failed(
            /*
            b"src/buffer.c\0" as *const u8 as *const libc::c_char,
            106 as libc::c_int as libc::c_uint,
            b"req_size >= b->used\0" as *const u8 as *const libc::c_char,
            */
        );
    }
    let used = (*b).used;
    return (buffer_realloc(b, req_size))
        .offset(used as isize)
        .offset(-(1 as libc::c_int as isize));
}
// CHECK-LABEL: fn buffer_string_prepare_append<'h0,'h1>(
#[no_mangle]
pub unsafe extern "C" fn buffer_string_prepare_append(
    // CHECK: b: &'h0 mut (buffer),
    b: *mut buffer,
    // CHECK: size: size_t,
    size: size_t,
// CHECK: ) -> {{.*}}Option<&'h1 (libc::c_char)> {
// This function's return value is currently unused.  If it were used, it would probably have this
// type instead:
// XXXXX: ) -> {{.*}}Option<&'h1 mut [(libc::c_char)]> {
) -> *mut libc::c_char {
    let len: uint32_t = if (*b).used != 0 {
        ((*b).used).wrapping_sub(1 as libc::c_int as libc::c_uint)
    } else {
        0 as libc::c_int as libc::c_uint
    };
    return if ((*b).size).wrapping_sub(len) as libc::c_ulong
        >= size.wrapping_add(1 as libc::c_int as libc::c_ulong)
    {
        ((*b).ptr).offset(len as isize)
    } else {
        buffer_string_prepare_append_resize(b, size)
    };
}
// CHECK-LABEL: fn buffer_extend<'h0,'h1>(b: &'h0 mut (buffer), x: size_t) -> {{.*}}Option<&'h1 (libc::c_char)> {
#[no_mangle]
#[inline(never)]
pub unsafe extern "C" fn buffer_extend(b: *mut buffer, x: size_t) -> *mut libc::c_char {
    let len: uint32_t = if (*b).used != 0 {
        ((*b).used).wrapping_sub(1 as libc::c_int as libc::c_uint)
    } else {
        0 as libc::c_int as libc::c_uint
    };
    (*b)
        .used = (len as libc::c_ulong)
        .wrapping_add(x)
        .wrapping_add(1 as libc::c_int as libc::c_ulong) as uint32_t;
    let s: *mut libc::c_char = if ((*b).size).wrapping_sub(len) as libc::c_ulong
        >= x.wrapping_add(1 as libc::c_int as libc::c_ulong)
    {
        ((*b).ptr).offset(len as isize)
    } else {
        buffer_string_prepare_append_resize(b, x)
    };
    *s.offset(x as isize) = '\u{0}' as i32 as libc::c_char;
    return s;
}
// CHECK-LABEL: fn buffer_commit<'h0>(mut b: &'h0 mut (buffer), mut size: size_t) {
#[no_mangle]
pub unsafe extern "C" fn buffer_commit(mut b: *mut buffer, mut size: size_t) {
    let mut sz: size_t = (*b).used as size_t;
    if 0 as libc::c_int as libc::c_ulong == sz {
        sz = 1 as libc::c_int as size_t;
    }
    if size > 0 as libc::c_int as libc::c_ulong {
        sz = (sz as libc::c_ulong).wrapping_add(size) as size_t as size_t;
        if !(sz > size) {
            ck_assert_failed(
                /*
                b"src/buffer.c\0" as *const u8 as *const libc::c_char,
                149 as libc::c_int as libc::c_uint,
                b"sz > size\0" as *const u8 as *const libc::c_char,
                */
            );
        }
    }
    (*b).used = sz as uint32_t;
    *((*b).ptr)
        .offset(
            sz.wrapping_sub(1 as libc::c_int as libc::c_ulong) as isize,
        ) = '\u{0}' as i32 as libc::c_char;
}
/*
// CHECK-LABEL: fn buffer_copy_string
#[no_mangle]
#[cold]
pub unsafe extern "C" fn buffer_copy_string(
    mut b: *mut buffer,
    mut s: *const libc::c_char,
) {
    if (0 as *mut libc::c_void as *const libc::c_char == s) as libc::c_int
        as libc::c_long != 0
    {
        s = b"\0" as *const u8 as *const libc::c_char;
    }
    buffer_copy_string_len(b, s, strlen(s));
}
#[no_mangle]
pub unsafe extern "C" fn buffer_copy_string_len(
    b: *mut buffer,
    s: *const libc::c_char,
    len: size_t,
) {
    (*b).used = len.wrapping_add(1 as libc::c_int as libc::c_ulong) as uint32_t;
    let d: *mut libc::c_char = if len < (*b).size as libc::c_ulong {
        (*b).ptr
    } else {
        buffer_alloc_replace(b, len)
    };
    *d.offset(len as isize) = '\u{0}' as i32 as libc::c_char;
    memcpy(d as *mut libc::c_void, s as *const libc::c_void, len);
}
#[no_mangle]
#[cold]
pub unsafe extern "C" fn buffer_append_string(
    mut b: *mut buffer,
    mut s: *const libc::c_char,
) {
    if (0 as *mut libc::c_void as *const libc::c_char == s) as libc::c_int
        as libc::c_long != 0
    {
        s = b"\0" as *const u8 as *const libc::c_char;
    }
    buffer_append_string_len(b, s, strlen(s));
}
#[no_mangle]
pub unsafe extern "C" fn buffer_append_string_len(
    b: *mut buffer,
    s: *const libc::c_char,
    len: size_t,
) {
    memcpy(buffer_extend(b, len) as *mut libc::c_void, s as *const libc::c_void, len);
}
*/
/* TODO: needs support for `mempcpy`
#[no_mangle]
pub unsafe extern "C" fn buffer_append_str2(
    b: *mut buffer,
    s1: *const libc::c_char,
    len1: size_t,
    s2: *const libc::c_char,
    len2: size_t,
) {
    let s: *mut libc::c_char = buffer_extend(b, len1.wrapping_add(len2));
    mempcpy(
        mempcpy(s as *mut libc::c_void, s1 as *const libc::c_void, len1),
        s2 as *const libc::c_void,
        len2,
    );
}
#[no_mangle]
pub unsafe extern "C" fn buffer_append_str3(
    b: *mut buffer,
    s1: *const libc::c_char,
    len1: size_t,
    s2: *const libc::c_char,
    len2: size_t,
    s3: *const libc::c_char,
    len3: size_t,
) {
    let mut s: *mut libc::c_char = buffer_extend(
        b,
        len1.wrapping_add(len2).wrapping_add(len3),
    );
    mempcpy(
        mempcpy(
            mempcpy(s as *mut libc::c_void, s1 as *const libc::c_void, len1),
            s2 as *const libc::c_void,
            len2,
        ),
        s3 as *const libc::c_void,
        len3,
    );
}
#[no_mangle]
pub unsafe extern "C" fn buffer_append_iovec(
    b: *mut buffer,
    iov: *const const_iovec,
    n: size_t,
) {
    let mut len: size_t = 0 as libc::c_int as size_t;
    let mut i: size_t = 0 as libc::c_int as size_t;
    while i < n {
        len = (len as libc::c_ulong).wrapping_add((*iov.offset(i as isize)).iov_len)
            as size_t as size_t;
        i = i.wrapping_add(1);
    }
    let mut s: *mut libc::c_char = buffer_extend(b, len);
    let mut i_0: size_t = 0 as libc::c_int as size_t;
    while i_0 < n {
        if !(0 as libc::c_int as libc::c_ulong == (*iov.offset(i_0 as isize)).iov_len) {
            s = mempcpy(
                s as *mut libc::c_void,
                (*iov.offset(i_0 as isize)).iov_base,
                (*iov.offset(i_0 as isize)).iov_len,
            ) as *mut libc::c_char;
        }
        i_0 = i_0.wrapping_add(1);
    }
}
*/
/*
#[no_mangle]
pub unsafe extern "C" fn buffer_append_path_len(
    mut b: *mut buffer,
    mut a: *const libc::c_char,
    mut alen: size_t,
) {
    let mut s: *mut libc::c_char = buffer_string_prepare_append(
        b,
        alen.wrapping_add(1 as libc::c_int as libc::c_ulong),
    );
    let aslash: libc::c_int = (alen != 0
        && *a.offset(0 as libc::c_int as isize) as libc::c_int == '/' as i32)
        as libc::c_int;
    if (*b).used > 1 as libc::c_int as libc::c_uint
        && *s.offset(-(1 as libc::c_int) as isize) as libc::c_int == '/' as i32
    {
        if aslash != 0 {
            a = a.offset(1);
            alen = alen.wrapping_sub(1);
        }
    } else {
        if 0 as libc::c_int as libc::c_uint == (*b).used {
            (*b).used = 1 as libc::c_int as uint32_t;
        }
        if aslash == 0 {
            let fresh0 = s;
            s = s.offset(1);
            *fresh0 = '/' as i32 as libc::c_char;
            (*b).used = ((*b).used).wrapping_add(1);
        }
    }
    (*b).used = ((*b).used as libc::c_ulong).wrapping_add(alen) as uint32_t as uint32_t;
    *s.offset(alen as isize) = '\u{0}' as i32 as libc::c_char;
    memcpy(s as *mut libc::c_void, a as *const libc::c_void, alen);
}
#[no_mangle]
pub unsafe extern "C" fn buffer_copy_path_len2(
    b: *mut buffer,
    s1: *const libc::c_char,
    mut len1: size_t,
    s2: *const libc::c_char,
    mut len2: size_t,
) {
    memcpy(
        buffer_string_prepare_copy(
            b,
            len1.wrapping_add(len2).wrapping_add(1 as libc::c_int as libc::c_ulong),
        ) as *mut libc::c_void,
        s1 as *const libc::c_void,
        len1,
    );
    (*b).used = len1.wrapping_add(1 as libc::c_int as libc::c_ulong) as uint32_t;
    buffer_append_path_len(b, s2, len2);
}
*/
// CHECK-LABEL: fn buffer_copy_string_len_lc<'h0,'h1>(
#[no_mangle]
pub unsafe extern "C" fn buffer_copy_string_len_lc(
    // CHECK: b: &'h0 mut (buffer),
    b: *mut buffer,
    // CHECK: s: &'h1 [(libc::c_char)],
    s: *const libc::c_char,
    // CHECK: len: size_t,
    len: size_t,
// CHECK: ) {
) {
    (*b).used = len.wrapping_add(1 as libc::c_int as libc::c_ulong) as uint32_t;
    let d: *mut libc::c_char = buffer_string_prepare_copy(b, len);
    *d.offset(len as isize) = '\u{0}' as i32 as libc::c_char;
    let mut i: size_t = 0 as libc::c_int as size_t;
    while i < len {
        *d
            .offset(
                i as isize,
            ) = (if !((*s.offset(i as isize) as uint32_t)
            .wrapping_sub('A' as i32 as libc::c_uint)
            <= ('Z' as i32 - 'A' as i32) as libc::c_uint)
        {
            *s.offset(i as isize) as libc::c_int
        } else {
            *s.offset(i as isize) as libc::c_int | 0x20 as libc::c_int
        }) as libc::c_char;
        i = i.wrapping_add(1);
    }
}
/*
// CHECK-LABEL: fn buffer_append_uint_hex_lc
#[no_mangle]
pub unsafe extern "C" fn buffer_append_uint_hex_lc(
    mut b: *mut buffer,
    mut value: uintmax_t,
) {
    let mut buf: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut shift: libc::c_uint = 0 as libc::c_int as libc::c_uint;
    let mut copy: uintmax_t = value;
    loop {
        copy >>= 8 as libc::c_int;
        shift = shift.wrapping_add(8 as libc::c_int as libc::c_uint);
        if !(0 as libc::c_int as libc::c_ulong != copy) {
            break;
        }
    }
    buf = buffer_extend(b, (shift >> 2 as libc::c_int) as size_t);
    while shift > 0 as libc::c_int as libc::c_uint {
        shift = shift.wrapping_sub(4 as libc::c_int as libc::c_uint);
        let fresh1 = buf;
        buf = buf.offset(1);
        *fresh1 = hex_chars_lc[(value >> shift & 0xf as libc::c_int as libc::c_ulong)
            as usize];
    }
}
*/
/*
unsafe extern "C" fn utostr(
    mut buf: *mut libc::c_char,
    mut val: uintmax_t,
) -> *mut libc::c_char {
    let mut cur: *mut libc::c_char = buf
        .offset(
            (2 as libc::c_int as libc::c_ulong)
                .wrapping_add(
                    (8 as libc::c_int as libc::c_ulong)
                        .wrapping_mul(::std::mem::size_of::<intmax_t>() as libc::c_ulong)
                        .wrapping_mul(31 as libc::c_int as libc::c_ulong)
                        .wrapping_add(99 as libc::c_int as libc::c_ulong)
                        .wrapping_div(100 as libc::c_int as libc::c_ulong),
                ) as isize,
        );
    let mut x: uintmax_t = 0;
    loop {
        x = val.wrapping_div(10 as libc::c_int as libc::c_ulong);
        cur = cur.offset(-1);
        *cur = ('0' as i32
            + val.wrapping_sub(x.wrapping_mul(10 as libc::c_int as libc::c_ulong))
                as libc::c_int) as libc::c_char;
        val = x;
        if !(0 as libc::c_int as libc::c_ulong != val) {
            break;
        }
    }
    return cur;
}
unsafe extern "C" fn itostr(
    mut buf: *mut libc::c_char,
    mut val: intmax_t,
) -> *mut libc::c_char {
    let mut uval: uintmax_t = if val >= 0 as libc::c_int as libc::c_long {
        val as uintmax_t
    } else {
        (!val as uintmax_t).wrapping_add(1 as libc::c_int as libc::c_ulong)
    };
    let mut cur: *mut libc::c_char = utostr(buf, uval);
    if val < 0 as libc::c_int as libc::c_long {
        cur = cur.offset(-1);
        *cur = '-' as i32 as libc::c_char;
    }
    return cur;
}
*/
/* TODO: offset_from
#[no_mangle]
pub unsafe extern "C" fn buffer_append_int(mut b: *mut buffer, mut val: intmax_t) {
    let mut buf: [libc::c_char; 22] = [0; 22];
    let str: *const libc::c_char = itostr(buf.as_mut_ptr(), val);
    buffer_append_string_len(
        b,
        str,
        buf
            .as_mut_ptr()
            .offset(
                ::std::mem::size_of::<[libc::c_char; 22]>() as libc::c_ulong as isize,
            )
            .offset_from(str) as libc::c_long as size_t,
    );
}
*/
/*
#[no_mangle]
pub unsafe extern "C" fn buffer_append_strftime(
    b: *mut buffer,
    format: *const libc::c_char,
    tm: *const tm,
) {
    if (0 as *mut libc::c_void as *const tm == tm) as libc::c_int as libc::c_long != 0 {
        return;
    }
    let mut rv: size_t = strftime(
        buffer_string_prepare_append(b, 63 as libc::c_int as size_t),
        64 as libc::c_int as size_t,
        format,
        tm,
    );
    if (0 as libc::c_int as libc::c_ulong == rv) as libc::c_int as libc::c_long != 0
        || (rv > 63 as libc::c_int as libc::c_ulong) as libc::c_int as libc::c_long != 0
    {
        rv = strftime(
            buffer_string_prepare_append(b, 4095 as libc::c_int as size_t),
            4096 as libc::c_int as size_t,
            format,
            tm,
        );
        if (rv > 4095 as libc::c_int as libc::c_ulong) as libc::c_int as libc::c_long
            != 0
        {
            return;
        }
    }
    (*b)
        .used = ((*b).used as libc::c_uint)
        .wrapping_add(
            (rv as uint32_t)
                .wrapping_add(
                    (0 as libc::c_int as libc::c_uint == (*b).used) as libc::c_int
                        as libc::c_uint,
                ),
        ) as uint32_t as uint32_t;
}
*/
/* TODO: offset_from
#[no_mangle]
pub unsafe extern "C" fn li_itostrn(
    mut buf: *mut libc::c_char,
    mut buf_len: size_t,
    mut val: intmax_t,
) -> size_t {
    let mut p_buf: [libc::c_char; 22] = [0; 22];
    let str: *mut libc::c_char = itostr(p_buf.as_mut_ptr(), val);
    let mut len: size_t = p_buf
        .as_mut_ptr()
        .offset(::std::mem::size_of::<[libc::c_char; 22]>() as libc::c_ulong as isize)
        .offset_from(str) as libc::c_long as size_t;
    if !(len <= buf_len) {
        ck_assert_failed(
            /*
            b"src/buffer.c\0" as *const u8 as *const libc::c_char,
            347 as libc::c_int as libc::c_uint,
            b"len <= buf_len\0" as *const u8 as *const libc::c_char,
            */
        );
    }
    memcpy(buf as *mut libc::c_void, str as *const libc::c_void, len);
    return len;
}
#[no_mangle]
pub unsafe extern "C" fn li_utostrn(
    mut buf: *mut libc::c_char,
    mut buf_len: size_t,
    mut val: uintmax_t,
) -> size_t {
    let mut p_buf: [libc::c_char; 22] = [0; 22];
    let str: *mut libc::c_char = utostr(p_buf.as_mut_ptr(), val);
    let mut len: size_t = p_buf
        .as_mut_ptr()
        .offset(::std::mem::size_of::<[libc::c_char; 22]>() as libc::c_ulong as isize)
        .offset_from(str) as libc::c_long as size_t;
    if !(len <= buf_len) {
        ck_assert_failed(
            /*
            b"src/buffer.c\0" as *const u8 as *const libc::c_char,
            356 as libc::c_int as libc::c_uint,
            b"len <= buf_len\0" as *const u8 as *const libc::c_char,
            */
        );
    }
    memcpy(buf as *mut libc::c_void, str as *const libc::c_void, len);
    return len;
}
*/
// CHECK-LABEL: fn hex2int(mut hex: libc::c_uchar) -> libc::c_char {
#[no_mangle]
pub unsafe extern "C" fn hex2int(mut hex: libc::c_uchar) -> libc::c_char {
    let mut n: libc::c_uchar = 0;
    n = (hex as libc::c_int - '0' as i32) as libc::c_uchar;
    return (if n as libc::c_int <= 9 as libc::c_int
        || {
            n = ((hex as libc::c_int & 0xdf as libc::c_int) - 'A' as i32)
                as libc::c_uchar;
            (if n as libc::c_int <= 5 as libc::c_int {
                n = (n as libc::c_int + 10 as libc::c_int) as libc::c_uchar;
                n as libc::c_int
            } else {
                0 as libc::c_int
            }) != 0
        }
    {
        n as libc::c_char as libc::c_int
    } else {
        0xff as libc::c_int
    }) as libc::c_char;
}
/* TODO: u8 pointee
// CHECK-LABEL: fn li_hex2bin
#[no_mangle]
pub unsafe extern "C" fn li_hex2bin(
    bin: *mut libc::c_uchar,
    binlen: size_t,
    hexstr: *const libc::c_char,
    len: size_t,
) -> libc::c_int {
    if len > binlen << 1 as libc::c_int {
        return -(1 as libc::c_int);
    }
    let mut i: libc::c_int = 0 as libc::c_int;
    let mut ilen: libc::c_int = len as libc::c_int;
    while i < ilen {
        let mut hi: libc::c_int = *hexstr.offset(i as isize) as libc::c_int;
        let mut lo: libc::c_int = *hexstr.offset((i + 1 as libc::c_int) as isize)
            as libc::c_int;
        if '0' as i32 <= hi && hi <= '9' as i32 {
            hi -= '0' as i32;
        } else {
            hi |= 0x20 as libc::c_int;
            if (hi as uint32_t).wrapping_sub('a' as i32 as libc::c_uint)
                <= ('f' as i32 - 'a' as i32) as libc::c_uint
            {
                hi += -('a' as i32) + 10 as libc::c_int;
            } else {
                return -(1 as libc::c_int)
            }
        }
        if '0' as i32 <= lo && lo <= '9' as i32 {
            lo -= '0' as i32;
        } else {
            lo |= 0x20 as libc::c_int;
            if (lo as uint32_t).wrapping_sub('a' as i32 as libc::c_uint)
                <= ('f' as i32 - 'a' as i32) as libc::c_uint
            {
                lo += -('a' as i32) + 10 as libc::c_int;
            } else {
                return -(1 as libc::c_int)
            }
        }
        *bin
            .offset(
                (i >> 1 as libc::c_int) as isize,
            ) = (hi << 4 as libc::c_int | lo) as libc::c_uchar;
        i += 2 as libc::c_int;
    }
    return 0 as libc::c_int;
}
*/
/* TODO: u8 pointee
#[no_mangle]
#[inline(never)]
pub unsafe extern "C" fn buffer_eq_icase_ssn(
    a: *const libc::c_char,
    b: *const libc::c_char,
    len: size_t,
) -> libc::c_int {
    let mut i: size_t = 0 as libc::c_int as size_t;
    while i < len {
        let mut ca: libc::c_uint = *(a as *mut libc::c_uchar).offset(i as isize)
            as libc::c_uint;
        let mut cb: libc::c_uint = *(b as *mut libc::c_uchar).offset(i as isize)
            as libc::c_uint;
        if ca != cb {
            ca |= 0x20 as libc::c_int as libc::c_uint;
            cb |= 0x20 as libc::c_int as libc::c_uint;
            if ca != cb {
                return 0 as libc::c_int;
            }
            if !(ca.wrapping_sub('a' as i32 as libc::c_uint)
                <= ('z' as i32 - 'a' as i32) as libc::c_uint)
            {
                return 0 as libc::c_int;
            }
            if !(cb.wrapping_sub('a' as i32 as libc::c_uint)
                <= ('z' as i32 - 'a' as i32) as libc::c_uint)
            {
                return 0 as libc::c_int;
            }
        }
        i = i.wrapping_add(1);
    }
    return 1 as libc::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn buffer_eq_icase_ss(
    a: *const libc::c_char,
    alen: size_t,
    b: *const libc::c_char,
    blen: size_t,
) -> libc::c_int {
    return if alen == blen { buffer_eq_icase_ssn(a, b, blen) } else { 0 as libc::c_int };
}
#[no_mangle]
pub unsafe extern "C" fn buffer_eq_icase_slen(
    b: *const buffer,
    s: *const libc::c_char,
    slen: size_t,
) -> libc::c_int {
    return if (*b).used as libc::c_ulong
        == slen.wrapping_add(1 as libc::c_int as libc::c_ulong)
    {
        buffer_eq_icase_ssn((*b).ptr, s, slen)
    } else {
        0 as libc::c_int
    };
}
#[no_mangle]
pub unsafe extern "C" fn buffer_eq_slen(
    b: *const buffer,
    s: *const libc::c_char,
    slen: size_t,
) -> libc::c_int {
    return ((*b).used as libc::c_ulong
        == slen.wrapping_add(1 as libc::c_int as libc::c_ulong)
        && 0 as libc::c_int
            == memcmp((*b).ptr as *const libc::c_void, s as *const libc::c_void, slen))
        as libc::c_int;
}
*/
/*
#[no_mangle]
pub unsafe extern "C" fn buffer_is_equal(
    mut a: *const buffer,
    mut b: *const buffer,
) -> libc::c_int {
    return ((*a).used == (*b).used
        && 0 as libc::c_int
            == memcmp(
                (*a).ptr as *const libc::c_void,
                (*b).ptr as *const libc::c_void,
                (*a).used as libc::c_ulong,
            )) as libc::c_int;
}
*/
/* TODO: uses hex_chars_lc/uc
#[no_mangle]
pub unsafe extern "C" fn li_tohex_lc(
    buf: *mut libc::c_char,
    mut buf_len: size_t,
    s: *const libc::c_char,
    mut s_len: size_t,
) {
    if !((2 as libc::c_int as libc::c_ulong).wrapping_mul(s_len) > s_len) {
        ck_assert_failed(
            /*
            b"src/buffer.c\0" as *const u8 as *const libc::c_char,
            440 as libc::c_int as libc::c_uint,
            b"2 * s_len > s_len\0" as *const u8 as *const libc::c_char,
            */
        );
    }
    if !((2 as libc::c_int as libc::c_ulong).wrapping_mul(s_len) < buf_len) {
        ck_assert_failed(
            /*
            b"src/buffer.c\0" as *const u8 as *const libc::c_char,
            441 as libc::c_int as libc::c_uint,
            b"2 * s_len < buf_len\0" as *const u8 as *const libc::c_char,
            */
        );
    }
    let mut i: size_t = 0 as libc::c_int as size_t;
    while i < s_len {
        *buf
            .offset(
                (2 as libc::c_int as libc::c_ulong).wrapping_mul(i) as isize,
            ) = hex_chars_lc[(*s.offset(i as isize) as libc::c_int >> 4 as libc::c_int
            & 0xf as libc::c_int) as usize];
        *buf
            .offset(
                (2 as libc::c_int as libc::c_ulong)
                    .wrapping_mul(i)
                    .wrapping_add(1 as libc::c_int as libc::c_ulong) as isize,
            ) = hex_chars_lc[(*s.offset(i as isize) as libc::c_int & 0xf as libc::c_int)
            as usize];
        i = i.wrapping_add(1);
    }
    *buf
        .offset(
            (2 as libc::c_int as libc::c_ulong).wrapping_mul(s_len) as isize,
        ) = '\u{0}' as i32 as libc::c_char;
}
#[no_mangle]
pub unsafe extern "C" fn li_tohex_uc(
    buf: *mut libc::c_char,
    mut buf_len: size_t,
    s: *const libc::c_char,
    mut s_len: size_t,
) {
    if !((2 as libc::c_int as libc::c_ulong).wrapping_mul(s_len) > s_len) {
        ck_assert_failed(
            /*
            b"src/buffer.c\0" as *const u8 as *const libc::c_char,
            451 as libc::c_int as libc::c_uint,
            b"2 * s_len > s_len\0" as *const u8 as *const libc::c_char,
            */
        );
    }
    if !((2 as libc::c_int as libc::c_ulong).wrapping_mul(s_len) < buf_len) {
        ck_assert_failed(
            /*
            b"src/buffer.c\0" as *const u8 as *const libc::c_char,
            452 as libc::c_int as libc::c_uint,
            b"2 * s_len < buf_len\0" as *const u8 as *const libc::c_char,
            */
        );
    }
    let mut i: size_t = 0 as libc::c_int as size_t;
    while i < s_len {
        *buf
            .offset(
                (2 as libc::c_int as libc::c_ulong).wrapping_mul(i) as isize,
            ) = hex_chars_uc[(*s.offset(i as isize) as libc::c_int >> 4 as libc::c_int
            & 0xf as libc::c_int) as usize];
        *buf
            .offset(
                (2 as libc::c_int as libc::c_ulong)
                    .wrapping_mul(i)
                    .wrapping_add(1 as libc::c_int as libc::c_ulong) as isize,
            ) = hex_chars_uc[(*s.offset(i as isize) as libc::c_int & 0xf as libc::c_int)
            as usize];
        i = i.wrapping_add(1);
    }
    *buf
        .offset(
            (2 as libc::c_int as libc::c_ulong).wrapping_mul(s_len) as isize,
        ) = '\u{0}' as i32 as libc::c_char;
}
*/
/*
#[no_mangle]
pub unsafe extern "C" fn buffer_substr_replace(
    b: *mut buffer,
    offset: size_t,
    len: size_t,
    replace: *const buffer,
) {
    let blen: size_t = buffer_clen(b) as size_t;
    let rlen: size_t = buffer_clen(replace) as size_t;
    if rlen > len {
        buffer_extend(b, blen.wrapping_sub(len).wrapping_add(rlen));
        memmove(
            ((*b).ptr).offset(offset as isize).offset(rlen as isize)
                as *mut libc::c_void,
            ((*b).ptr).offset(offset as isize).offset(len as isize)
                as *const libc::c_void,
            blen.wrapping_sub(offset).wrapping_sub(len),
        );
    }
    memcpy(
        ((*b).ptr).offset(offset as isize) as *mut libc::c_void,
        (*replace).ptr as *const libc::c_void,
        rlen,
    );
    if rlen < len {
        memmove(
            ((*b).ptr).offset(offset as isize).offset(rlen as isize)
                as *mut libc::c_void,
            ((*b).ptr).offset(offset as isize).offset(len as isize)
                as *const libc::c_void,
            blen.wrapping_sub(offset).wrapping_sub(len),
        );
        buffer_truncate(b, blen.wrapping_sub(len).wrapping_add(rlen) as uint32_t);
    }
}
*/
/* TODO: u8 pointee
#[no_mangle]
pub unsafe extern "C" fn buffer_append_string_encoded_hex_lc(
    b: *mut buffer,
    s: *const libc::c_char,
    mut len: size_t,
) {
    let p: *mut libc::c_uchar = buffer_extend(
        b,
        len.wrapping_mul(2 as libc::c_int as libc::c_ulong),
    ) as *mut libc::c_uchar;
    let mut i: size_t = 0 as libc::c_int as size_t;
    while i < len {
        *p
            .offset(
                (i << 1 as libc::c_int) as isize,
            ) = hex_chars_lc[(*s.offset(i as isize) as libc::c_int >> 4 as libc::c_int
            & 0xf as libc::c_int) as usize] as libc::c_uchar;
        *p
            .offset(
                (i << 1 as libc::c_int).wrapping_add(1 as libc::c_int as libc::c_ulong)
                    as isize,
            ) = hex_chars_lc[(*s.offset(i as isize) as libc::c_int & 0xf as libc::c_int)
            as usize] as libc::c_uchar;
        i = i.wrapping_add(1);
    }
}
#[no_mangle]
pub unsafe extern "C" fn buffer_append_string_encoded_hex_uc(
    b: *mut buffer,
    s: *const libc::c_char,
    mut len: size_t,
) {
    let p: *mut libc::c_uchar = buffer_extend(
        b,
        len.wrapping_mul(2 as libc::c_int as libc::c_ulong),
    ) as *mut libc::c_uchar;
    let mut i: size_t = 0 as libc::c_int as size_t;
    while i < len {
        *p
            .offset(
                (i << 1 as libc::c_int) as isize,
            ) = hex_chars_uc[(*s.offset(i as isize) as libc::c_int >> 4 as libc::c_int
            & 0xf as libc::c_int) as usize] as libc::c_uchar;
        *p
            .offset(
                (i << 1 as libc::c_int).wrapping_add(1 as libc::c_int as libc::c_ulong)
                    as isize,
            ) = hex_chars_uc[(*s.offset(i as isize) as libc::c_int & 0xf as libc::c_int)
            as usize] as libc::c_uchar;
        i = i.wrapping_add(1);
    }
}
*/
static mut encoded_chars_rel_uri_part: [libc::c_char; 256] = [
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
];
static mut encoded_chars_rel_uri: [libc::c_char; 256] = [
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
];
static mut encoded_chars_html: [libc::c_char; 256] = [
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
];
static mut encoded_chars_minimal_xml: [libc::c_char; 256] = [
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    1 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
    0 as libc::c_int as libc::c_char,
];
/* TODO: u8 pointee
#[no_mangle]
pub unsafe extern "C" fn buffer_append_string_encoded(
    b: *mut buffer,
    s: *const libc::c_char,
    mut s_len: size_t,
    mut encoding: buffer_encoding_t,
) {
    let mut ds: *mut libc::c_uchar = 0 as *mut libc::c_uchar;
    let mut d: *mut libc::c_uchar = 0 as *mut libc::c_uchar;
    let mut d_len: size_t = 0;
    let mut ndx: size_t = 0;
    let mut map: *const libc::c_char = 0 as *const libc::c_char;
    match encoding as libc::c_uint {
        0 => {
            map = encoded_chars_rel_uri.as_ptr();
        }
        1 => {
            map = encoded_chars_rel_uri_part.as_ptr();
        }
        2 => {
            map = encoded_chars_html.as_ptr();
        }
        3 => {
            map = encoded_chars_minimal_xml.as_ptr();
        }
        _ => {}
    }
    ds = s as *mut libc::c_uchar;
    d_len = 0 as libc::c_int as size_t;
    ndx = 0 as libc::c_int as size_t;
    while ndx < s_len {
        if *map.offset((*ds as libc::c_int & 0xff as libc::c_int) as isize) != 0 {
            match encoding as libc::c_uint {
                0 | 1 => {
                    d_len = (d_len as libc::c_ulong)
                        .wrapping_add(3 as libc::c_int as libc::c_ulong) as size_t
                        as size_t;
                }
                2 | 3 => {
                    d_len = (d_len as libc::c_ulong)
                        .wrapping_add(6 as libc::c_int as libc::c_ulong) as size_t
                        as size_t;
                }
                _ => {}
            }
        } else {
            d_len = d_len.wrapping_add(1);
        }
        ds = ds.offset(1);
        ndx = ndx.wrapping_add(1);
    }
    d = buffer_extend(b, d_len) as *mut libc::c_uchar;
    if d_len == s_len {
        memcpy(d as *mut libc::c_void, s as *const libc::c_void, s_len);
        return;
    }
    ds = s as *mut libc::c_uchar;
    d_len = 0 as libc::c_int as size_t;
    ndx = 0 as libc::c_int as size_t;
    while ndx < s_len {
        if *map.offset((*ds as libc::c_int & 0xff as libc::c_int) as isize) != 0 {
            match encoding as libc::c_uint {
                0 | 1 => {
                    let fresh2 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d.offset(fresh2 as isize) = '%' as i32 as libc::c_uchar;
                    let fresh3 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d
                        .offset(
                            fresh3 as isize,
                        ) = hex_chars_uc[(*ds as libc::c_int >> 4 as libc::c_int
                        & 0xf as libc::c_int) as usize] as libc::c_uchar;
                    let fresh4 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d
                        .offset(
                            fresh4 as isize,
                        ) = hex_chars_uc[(*ds as libc::c_int & 0xf as libc::c_int)
                        as usize] as libc::c_uchar;
                }
                2 | 3 => {
                    let fresh5 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d.offset(fresh5 as isize) = '&' as i32 as libc::c_uchar;
                    let fresh6 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d.offset(fresh6 as isize) = '#' as i32 as libc::c_uchar;
                    let fresh7 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d.offset(fresh7 as isize) = 'x' as i32 as libc::c_uchar;
                    let fresh8 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d
                        .offset(
                            fresh8 as isize,
                        ) = hex_chars_uc[(*ds as libc::c_int >> 4 as libc::c_int
                        & 0xf as libc::c_int) as usize] as libc::c_uchar;
                    let fresh9 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d
                        .offset(
                            fresh9 as isize,
                        ) = hex_chars_uc[(*ds as libc::c_int & 0xf as libc::c_int)
                        as usize] as libc::c_uchar;
                    let fresh10 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d.offset(fresh10 as isize) = ';' as i32 as libc::c_uchar;
                }
                _ => {}
            }
        } else {
            let fresh11 = d_len;
            d_len = d_len.wrapping_add(1);
            *d.offset(fresh11 as isize) = *ds;
        }
        ds = ds.offset(1);
        ndx = ndx.wrapping_add(1);
    }
}
#[no_mangle]
pub unsafe extern "C" fn buffer_append_string_encoded_json(
    b: *mut buffer,
    s: *const libc::c_char,
    len: size_t,
) {
    let ds: *const libc::c_uchar = s as *mut libc::c_uchar;
    let mut dlen: size_t = 0 as libc::c_int as size_t;
    let mut i: size_t = 0 as libc::c_int as size_t;
    while i < len {
        let mut c: libc::c_int = *ds.offset(i as isize) as libc::c_int;
        if c == '"' as i32 || c == '\\' as i32 || c < 0x20 as libc::c_int
            || c == 0x7f as libc::c_int
        {
            match c {
                8 | 9 | 10 | 12 | 13 | 34 | 92 => {
                    dlen = (dlen as libc::c_ulong)
                        .wrapping_add(2 as libc::c_int as libc::c_ulong) as size_t
                        as size_t;
                }
                _ => {
                    dlen = (dlen as libc::c_ulong)
                        .wrapping_add(6 as libc::c_int as libc::c_ulong) as size_t
                        as size_t;
                }
            }
        } else {
            dlen = dlen.wrapping_add(1);
        }
        i = i.wrapping_add(1);
    }
    let d: *mut libc::c_uchar = buffer_extend(b, dlen) as *mut libc::c_uchar;
    if (dlen == len) as libc::c_int as libc::c_long != 0 {
        memcpy(d as *mut libc::c_void, ds as *const libc::c_void, len);
        return;
    }
    dlen = 0 as libc::c_int as size_t;
    let mut i_0: size_t = 0 as libc::c_int as size_t;
    while i_0 < len {
        let mut c_0: libc::c_int = *ds.offset(i_0 as isize) as libc::c_int;
        if c_0 == '"' as i32 || c_0 == '\\' as i32 || c_0 < 0x20 as libc::c_int
            || c_0 == 0x7f as libc::c_int
        {
            let fresh12 = dlen;
            dlen = dlen.wrapping_add(1);
            *d.offset(fresh12 as isize) = '\\' as i32 as libc::c_uchar;
            match c_0 {
                8 => {
                    let fresh13 = dlen;
                    dlen = dlen.wrapping_add(1);
                    *d.offset(fresh13 as isize) = 'b' as i32 as libc::c_uchar;
                }
                9 => {
                    let fresh14 = dlen;
                    dlen = dlen.wrapping_add(1);
                    *d.offset(fresh14 as isize) = 't' as i32 as libc::c_uchar;
                }
                10 => {
                    let fresh15 = dlen;
                    dlen = dlen.wrapping_add(1);
                    *d.offset(fresh15 as isize) = 'n' as i32 as libc::c_uchar;
                }
                12 => {
                    let fresh16 = dlen;
                    dlen = dlen.wrapping_add(1);
                    *d.offset(fresh16 as isize) = 'f' as i32 as libc::c_uchar;
                }
                13 => {
                    let fresh17 = dlen;
                    dlen = dlen.wrapping_add(1);
                    *d.offset(fresh17 as isize) = 'r' as i32 as libc::c_uchar;
                }
                34 => {
                    let fresh18 = dlen;
                    dlen = dlen.wrapping_add(1);
                    *d.offset(fresh18 as isize) = '"' as i32 as libc::c_uchar;
                }
                92 => {
                    let fresh19 = dlen;
                    dlen = dlen.wrapping_add(1);
                    *d.offset(fresh19 as isize) = '\\' as i32 as libc::c_uchar;
                }
                _ => {
                    *d.offset(dlen as isize) = 'u' as i32 as libc::c_uchar;
                    *d
                        .offset(
                            dlen.wrapping_add(1 as libc::c_int as libc::c_ulong) as isize,
                        ) = '0' as i32 as libc::c_uchar;
                    *d
                        .offset(
                            dlen.wrapping_add(2 as libc::c_int as libc::c_ulong) as isize,
                        ) = '0' as i32 as libc::c_uchar;
                    *d
                        .offset(
                            dlen.wrapping_add(3 as libc::c_int as libc::c_ulong) as isize,
                        ) = hex_chars_lc[(c_0 >> 4 as libc::c_int & 0xf as libc::c_int)
                        as usize] as libc::c_uchar;
                    *d
                        .offset(
                            dlen.wrapping_add(4 as libc::c_int as libc::c_ulong) as isize,
                        ) = hex_chars_lc[(c_0 & 0xf as libc::c_int) as usize]
                        as libc::c_uchar;
                    dlen = (dlen as libc::c_ulong)
                        .wrapping_add(5 as libc::c_int as libc::c_ulong) as size_t
                        as size_t;
                }
            }
        } else {
            let fresh20 = dlen;
            dlen = dlen.wrapping_add(1);
            *d.offset(fresh20 as isize) = c_0 as libc::c_uchar;
        }
        i_0 = i_0.wrapping_add(1);
    }
}
#[no_mangle]
pub unsafe extern "C" fn buffer_append_string_c_escaped(
    b: *mut buffer,
    s: *const libc::c_char,
    mut s_len: size_t,
) {
    let mut ds: *mut libc::c_uchar = 0 as *mut libc::c_uchar;
    let mut d: *mut libc::c_uchar = 0 as *mut libc::c_uchar;
    let mut d_len: size_t = 0;
    let mut ndx: size_t = 0;
    ds = s as *mut libc::c_uchar;
    d_len = 0 as libc::c_int as size_t;
    ndx = 0 as libc::c_int as size_t;
    while ndx < s_len {
        if (*ds as libc::c_int) < 0x20 as libc::c_int
            || *ds as libc::c_int >= 0x7f as libc::c_int
        {
            match *ds as libc::c_int {
                9 | 13 | 10 => {
                    d_len = (d_len as libc::c_ulong)
                        .wrapping_add(2 as libc::c_int as libc::c_ulong) as size_t
                        as size_t;
                }
                _ => {
                    d_len = (d_len as libc::c_ulong)
                        .wrapping_add(4 as libc::c_int as libc::c_ulong) as size_t
                        as size_t;
                }
            }
        } else {
            d_len = d_len.wrapping_add(1);
        }
        ds = ds.offset(1);
        ndx = ndx.wrapping_add(1);
    }
    d = buffer_extend(b, d_len) as *mut libc::c_uchar;
    if d_len == s_len {
        memcpy(d as *mut libc::c_void, s as *const libc::c_void, s_len);
        return;
    }
    ds = s as *mut libc::c_uchar;
    d_len = 0 as libc::c_int as size_t;
    ndx = 0 as libc::c_int as size_t;
    while ndx < s_len {
        if (*ds as libc::c_int) < 0x20 as libc::c_int
            || *ds as libc::c_int >= 0x7f as libc::c_int
        {
            let fresh21 = d_len;
            d_len = d_len.wrapping_add(1);
            *d.offset(fresh21 as isize) = '\\' as i32 as libc::c_uchar;
            match *ds as libc::c_int {
                9 => {
                    let fresh22 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d.offset(fresh22 as isize) = 't' as i32 as libc::c_uchar;
                }
                13 => {
                    let fresh23 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d.offset(fresh23 as isize) = 'r' as i32 as libc::c_uchar;
                }
                10 => {
                    let fresh24 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d.offset(fresh24 as isize) = 'n' as i32 as libc::c_uchar;
                }
                _ => {
                    let fresh25 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d.offset(fresh25 as isize) = 'x' as i32 as libc::c_uchar;
                    let fresh26 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d
                        .offset(
                            fresh26 as isize,
                        ) = hex_chars_lc[(*ds as libc::c_int >> 4 as libc::c_int
                        & 0xf as libc::c_int) as usize] as libc::c_uchar;
                    let fresh27 = d_len;
                    d_len = d_len.wrapping_add(1);
                    *d
                        .offset(
                            fresh27 as isize,
                        ) = hex_chars_lc[(*ds as libc::c_int & 0xf as libc::c_int)
                        as usize] as libc::c_uchar;
                }
            }
        } else {
            let fresh28 = d_len;
            d_len = d_len.wrapping_add(1);
            *d.offset(fresh28 as isize) = *ds;
        }
        ds = ds.offset(1);
        ndx = ndx.wrapping_add(1);
    }
}
*/
/* TODO: offset_from
#[no_mangle]
pub unsafe extern "C" fn buffer_urldecode_path(b: *mut buffer) {
    let len: size_t = buffer_clen(b) as size_t;
    let mut src: *mut libc::c_char = (if len != 0 {
        memchr((*b).ptr as *const libc::c_void, '%' as i32, len)
    } else {
        0 as *mut libc::c_void
    }) as *mut libc::c_char;
    if src.is_null() {
        return;
    }
    let mut dst: *mut libc::c_char = src;
    loop {
        let mut high: libc::c_uchar = *(src as *mut libc::c_uchar)
            .offset(1 as libc::c_int as isize);
        let mut low: libc::c_uchar = (if high as libc::c_int != 0 {
            hex2int(*(src as *mut libc::c_uchar).offset(2 as libc::c_int as isize))
                as libc::c_int
        } else {
            0xff as libc::c_int
        }) as libc::c_uchar;
        high = hex2int(high) as libc::c_uchar;
        if 0xff as libc::c_int != high as libc::c_int
            && 0xff as libc::c_int != low as libc::c_int
        {
            high = ((high as libc::c_int) << 4 as libc::c_int | low as libc::c_int)
                as libc::c_uchar;
            *dst = (if high as libc::c_int >= 32 as libc::c_int
                && high as libc::c_int != 127 as libc::c_int
            {
                high as libc::c_int
            } else {
                '_' as i32
            }) as libc::c_char;
            src = src.offset(2 as libc::c_int as isize);
        }
        loop {
            src = src.offset(1);
            dst = dst.offset(1);
            *dst = *src;
            if !(*dst as libc::c_int != '%' as i32 && *src as libc::c_int != 0) {
                break;
            }
        }
        if !(*src != 0) {
            break;
        }
    }
    (*b)
        .used = (dst.offset_from((*b).ptr) as libc::c_long
        + 1 as libc::c_int as libc::c_long) as uint32_t;
}
*/
/* TODO: u8 pointee
#[no_mangle]
pub unsafe extern "C" fn buffer_is_valid_UTF8(mut b: *const buffer) -> libc::c_int {
    let mut c: *const libc::c_uchar = (*b).ptr as *mut libc::c_uchar;
    while *c != 0 {
        if (*c.offset(0 as libc::c_int as isize) as libc::c_int) < 0x80 as libc::c_int {
            c = c.offset(1);
        } else if 0xc2 as libc::c_int
                <= *c.offset(0 as libc::c_int as isize) as libc::c_int
                && *c.offset(0 as libc::c_int as isize) as libc::c_int
                    <= 0xdf as libc::c_int
                && 0x80 as libc::c_int
                    <= *c.offset(1 as libc::c_int as isize) as libc::c_int
                && *c.offset(1 as libc::c_int as isize) as libc::c_int
                    <= 0xbf as libc::c_int
            {
            c = c.offset(2 as libc::c_int as isize);
        } else if (0xe0 as libc::c_int
                == *c.offset(0 as libc::c_int as isize) as libc::c_int
                && 0xa0 as libc::c_int
                    <= *c.offset(1 as libc::c_int as isize) as libc::c_int
                && *c.offset(1 as libc::c_int as isize) as libc::c_int
                    <= 0xbf as libc::c_int
                || 0xe1 as libc::c_int
                    <= *c.offset(0 as libc::c_int as isize) as libc::c_int
                    && *c.offset(0 as libc::c_int as isize) as libc::c_int
                        <= 0xef as libc::c_int
                    && *c.offset(0 as libc::c_int as isize) as libc::c_int
                        != 0xed as libc::c_int
                    && 0x80 as libc::c_int
                        <= *c.offset(1 as libc::c_int as isize) as libc::c_int
                    && *c.offset(1 as libc::c_int as isize) as libc::c_int
                        <= 0xbf as libc::c_int
                || 0xed as libc::c_int
                    == *c.offset(0 as libc::c_int as isize) as libc::c_int
                    && 0x80 as libc::c_int
                        <= *c.offset(1 as libc::c_int as isize) as libc::c_int
                    && *c.offset(1 as libc::c_int as isize) as libc::c_int
                        <= 0x9f as libc::c_int)
                && 0x80 as libc::c_int
                    <= *c.offset(2 as libc::c_int as isize) as libc::c_int
                && *c.offset(2 as libc::c_int as isize) as libc::c_int
                    <= 0xbf as libc::c_int
            {
            c = c.offset(3 as libc::c_int as isize);
        } else if (0xf0 as libc::c_int
                == *c.offset(0 as libc::c_int as isize) as libc::c_int
                && 0x90 as libc::c_int
                    <= *c.offset(1 as libc::c_int as isize) as libc::c_int
                && *c.offset(1 as libc::c_int as isize) as libc::c_int
                    <= 0xbf as libc::c_int
                || 0xf1 as libc::c_int
                    <= *c.offset(0 as libc::c_int as isize) as libc::c_int
                    && *c.offset(0 as libc::c_int as isize) as libc::c_int
                        <= 0xf3 as libc::c_int
                    && 0x80 as libc::c_int
                        <= *c.offset(1 as libc::c_int as isize) as libc::c_int
                    && *c.offset(1 as libc::c_int as isize) as libc::c_int
                        <= 0xbf as libc::c_int
                || 0xf4 as libc::c_int
                    == *c.offset(0 as libc::c_int as isize) as libc::c_int
                    && 0x80 as libc::c_int
                        <= *c.offset(1 as libc::c_int as isize) as libc::c_int
                    && *c.offset(1 as libc::c_int as isize) as libc::c_int
                        <= 0x8f as libc::c_int)
                && 0x80 as libc::c_int
                    <= *c.offset(2 as libc::c_int as isize) as libc::c_int
                && *c.offset(2 as libc::c_int as isize) as libc::c_int
                    <= 0xbf as libc::c_int
                && 0x80 as libc::c_int
                    <= *c.offset(3 as libc::c_int as isize) as libc::c_int
                && *c.offset(3 as libc::c_int as isize) as libc::c_int
                    <= 0xbf as libc::c_int
            {
            c = c.offset(4 as libc::c_int as isize);
        } else {
            return 0 as libc::c_int
        }
    }
    return 1 as libc::c_int;
}
*/
/* TODO: offset_from
#[no_mangle]
pub unsafe extern "C" fn buffer_path_simplify(mut b: *mut buffer) {
    let mut out: *mut libc::c_char = (*b).ptr;
    let end: *mut libc::c_char = ((*b).ptr)
        .offset((*b).used as isize)
        .offset(-(1 as libc::c_int as isize));
    if buffer_is_blank(b) as libc::c_long != 0 {
        buffer_blank(b);
        return;
    }
    *end = '/' as i32 as libc::c_char;
    let mut walk: *mut libc::c_char = out;
    if (*walk as libc::c_int == '/' as i32) as libc::c_int as libc::c_long != 0 {
        loop {
            walk = walk.offset(1);
            if *walk as libc::c_int == '.' as i32 || *walk as libc::c_int == '/' as i32 {
                break;
            }
            loop {
                walk = walk.offset(1);
                if !(*walk as libc::c_int != '/' as i32) {
                    break;
                }
            }
            if !(walk != end) {
                break;
            }
        }
        if (walk == end) as libc::c_int as libc::c_long != 0 {
            *end = '\u{0}' as i32 as libc::c_char;
            return;
        }
        out = walk.offset(-(1 as libc::c_int as isize));
    } else {
        if *walk.offset(0 as libc::c_int as isize) as libc::c_int == '.' as i32
            && *walk.offset(1 as libc::c_int as isize) as libc::c_int == '/' as i32
        {
            walk = walk.offset(1);
            *out = *walk;
        } else if *walk.offset(0 as libc::c_int as isize) as libc::c_int == '.' as i32
                && *walk.offset(1 as libc::c_int as isize) as libc::c_int == '.' as i32
                && *walk.offset(2 as libc::c_int as isize) as libc::c_int == '/' as i32
            {
            walk = walk.offset(2 as libc::c_int as isize);
            *out = *walk;
        } else {
            loop {
                walk = walk.offset(1);
                if !(*walk as libc::c_int != '/' as i32) {
                    break;
                }
            }
            out = walk;
        }
        walk = walk.offset(1);
    }
    while walk <= end {
        if (*walk.offset(0 as libc::c_int as isize) as libc::c_int == '/' as i32)
            as libc::c_int as libc::c_long != 0
        {
            walk = walk.offset(1);
            if walk < end {
                continue;
            }
            out = out.offset(1);
            break;
        } else {
            if (*walk.offset(0 as libc::c_int as isize) as libc::c_int == '.' as i32)
                as libc::c_int as libc::c_long != 0
            {
                if *walk.offset(1 as libc::c_int as isize) as libc::c_int == '.' as i32
                    && *walk.offset(2 as libc::c_int as isize) as libc::c_int
                        == '/' as i32
                {
                    while out > (*b).ptr
                        && {
                            out = out.offset(-1);
                            *out as libc::c_int != '/' as i32
                        }
                    {}
                    *out = '/' as i32 as libc::c_char;
                    walk = walk.offset(3 as libc::c_int as isize);
                    if !(walk >= end) {
                        continue;
                    }
                    out = out.offset(1);
                    break;
                } else if *walk.offset(1 as libc::c_int as isize) as libc::c_int
                        == '/' as i32
                    {
                    walk = walk.offset(2 as libc::c_int as isize);
                    if !(walk >= end) {
                        continue;
                    }
                    out = out.offset(1);
                    break;
                } else {
                    out = out.offset(1);
                    *out = '.' as i32 as libc::c_char;
                    walk = walk.offset(1);
                }
            }
            loop {
                let fresh29 = walk;
                walk = walk.offset(1);
                out = out.offset(1);
                *out = *fresh29;
                if !(*out as libc::c_int != '/' as i32) {
                    break;
                }
            }
        }
    }
    *end = '\u{0}' as i32 as libc::c_char;
    *out = *end;
    (*b)
        .used = (out.offset_from((*b).ptr) as libc::c_long
        + 1 as libc::c_int as libc::c_long) as uint32_t;
}
*/
/* TODO: u8 pointee
#[no_mangle]
pub unsafe extern "C" fn buffer_to_lower(b: *mut buffer) {
    let s: *mut libc::c_uchar = (*b).ptr as *mut libc::c_uchar;
    let used: uint_fast32_t = (*b).used as uint_fast32_t;
    let mut i: uint_fast32_t = 0 as libc::c_int as uint_fast32_t;
    while i < used {
        if (*s.offset(i as isize) as uint32_t).wrapping_sub('A' as i32 as libc::c_uint)
            <= ('Z' as i32 - 'A' as i32) as libc::c_uint
        {
            // TODO: ref mut fresh
            //let ref mut fresh30 = *s.offset(i as isize);
            let fresh30 = &mut *s.offset(i as isize);
            *fresh30 = (*fresh30 as libc::c_int | 0x20 as libc::c_int) as libc::c_uchar;
        }
        i = i.wrapping_add(1);
    }
}
#[no_mangle]
pub unsafe extern "C" fn buffer_to_upper(b: *mut buffer) {
    let s: *mut libc::c_uchar = (*b).ptr as *mut libc::c_uchar;
    let used: uint_fast32_t = (*b).used as uint_fast32_t;
    let mut i: uint_fast32_t = 0 as libc::c_int as uint_fast32_t;
    while i < used {
        if (*s.offset(i as isize) as uint32_t).wrapping_sub('a' as i32 as libc::c_uint)
            <= ('z' as i32 - 'a' as i32) as libc::c_uint
        {
            // TODO: ref mut fresh
            //let ref mut fresh31 = *s.offset(i as isize);
            let fresh31 = &mut *s.offset(i as isize);
            *fresh31 = (*fresh31 as libc::c_int & 0xdf as libc::c_int) as libc::c_uchar;
        }
        i = i.wrapping_add(1);
    }
}
*/



#[cold]
unsafe extern "C" fn dummy_http_response_errdoc_init() {
    let mut www_auth: *mut buffer = 0 as *mut buffer;
    if true {
        if true {
            www_auth = buffer_init();
            //buffer_copy_buffer(www_auth, vb);
        }
    }
    //buffer_reset(&mut (*r).physical.path);
    let ptr = buffer_realloc(www_auth, 10);
    *ptr = 123;
    if !www_auth.is_null() {
        /*
        http_header_response_set(
            r,
            HTTP_HEADER_WWW_AUTHENTICATE,
            b"WWW-Authenticate\0" as *const u8 as *const libc::c_char,
            (::core::mem::size_of::<[libc::c_char; 17]>() as libc::c_ulong as uint32_t)
                .wrapping_sub(1 as libc::c_int as uint32_t),
            (*www_auth).ptr,
            buffer_clen(www_auth),
        );
        */
        buffer_free(www_auth);
    }
}
