use libc;
#[cfg(
    not(
        source_header = "/home/miguelsaldivar/workspace/C2Rust/dependencies/llvm-6.0.1/build.donna/lib/clang/6.0.1/include/stddef.h"
    )
)]
pub mod stddef_h {
    pub type size_t = libc::c_ulong;
    use super::libc;
}
#[cfg(not(source_header = "/usr/include/x86_64-linux-gnu/bits/types.h"))]
pub mod types_h {
    pub type __ssize_t = libc::c_long;
    use super::libc;
}
#[cfg(not(source_header = "/usr/include/ctype.h"))]
pub mod ctype_h {
    pub type unnamed = libc::c_uint;
    pub const _ISalnum: unnamed = 8;
    pub const _ISpunct: unnamed = 4;
    pub const _IScntrl: unnamed = 2;
    pub const _ISblank: unnamed = 1;
    pub const _ISgraph: unnamed = 32768;
    pub const _ISprint: unnamed = 16384;
    pub const _ISspace: unnamed = 8192;
    pub const _ISxdigit: unnamed = 4096;
    pub const _ISdigit: unnamed = 2048;
    pub const _ISalpha: unnamed = 1024;
    pub const _ISlower: unnamed = 512;
    pub const _ISupper: unnamed = 256;
    use super::libc;
    extern "C" {
        #[no_mangle]
        pub fn __ctype_b_loc() -> *mut *const libc::c_ushort;
    }
}
#[cfg(not(source_header = "/usr/include/x86_64-linux-gnu/sys/types.h"))]
pub mod sys_types_h {
    pub type ssize_t = __ssize_t;
    use super::types_h::__ssize_t;
}
#[cfg(not(source_header = "/home/miguelsaldivar/workspace/misc/buffer/buffer.h"))]
pub mod buffer_h {
    //
    // buffer.h
    //
    // Copyright (c) 2012 TJ Holowaychuk <tj@vision-media.ca>
    //
    /*
     * Default buffer size.
     */
    /*
     * Buffer struct.
     */
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct buffer_t {
        pub len: size_t,
        pub alloc: *mut libc::c_char,
        pub data: *mut libc::c_char,
    }
    use super::libc;
    use super::stddef_h::size_t;
    use super::sys_types_h::ssize_t;
}
#[cfg(not(source_header = "/usr/include/string.h"))]
pub mod string_h {
    use super::libc;
    extern "C" {
        #[no_mangle]
        pub fn memcpy(
            _: *mut libc::c_void,
            _: *const libc::c_void,
            _: libc::c_ulong,
        ) -> *mut libc::c_void;
        #[no_mangle]
        pub fn memmove(
            _: *mut libc::c_void,
            _: *const libc::c_void,
            _: libc::c_ulong,
        ) -> *mut libc::c_void;
        #[no_mangle]
        pub fn memset(_: *mut libc::c_void, _: libc::c_int, _: libc::c_ulong) -> *mut libc::c_void;
        #[no_mangle]
        pub fn strncat(
            _: *mut libc::c_char,
            _: *const libc::c_char,
            _: libc::c_ulong,
        ) -> *mut libc::c_char;
        #[no_mangle]
        pub fn strcmp(_: *const libc::c_char, _: *const libc::c_char) -> libc::c_int;
        #[no_mangle]
        pub fn strstr(_: *const libc::c_char, _: *const libc::c_char) -> *mut libc::c_char;
        #[no_mangle]
        pub fn strlen(_: *const libc::c_char) -> libc::c_ulong;
    }
}
#[cfg(not(source_header = "/usr/include/stdio.h"))]
pub mod stdio_h {
    use super::libc;
    extern "C" {
        #[no_mangle]
        pub fn printf(_: *const libc::c_char, ...) -> libc::c_int;
    }
}
#[cfg(not(source_header = "/usr/include/stdlib.h"))]
pub mod stdlib_h {
    use super::libc;
    extern "C" {
        #[no_mangle]
        pub fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
        #[no_mangle]
        pub fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
        #[no_mangle]
        pub fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
        #[no_mangle]
        pub fn free(__ptr: *mut libc::c_void) -> ();
    }
}
use self::buffer_h::buffer_t;
use self::ctype_h::{
    _ISalnum, _ISalpha, _ISblank, _IScntrl, _ISdigit, _ISgraph, _ISlower, _ISprint, _ISpunct,
    _ISspace, _ISupper, _ISxdigit, __ctype_b_loc, unnamed,
};
use self::stddef_h::size_t;
use self::stdio_h::printf;
use self::stdlib_h::{calloc, free, malloc, realloc};
use self::string_h::{memcpy, memmove, memset, strcmp, strlen, strncat, strstr};
use self::sys_types_h::ssize_t;
use self::types_h::__ssize_t;
// prototypes
#[no_mangle]
pub unsafe extern "C" fn buffer_new() -> *mut buffer_t {
    return buffer_new_with_size(64i32 as size_t);
}
#[no_mangle]
pub unsafe extern "C" fn buffer_new_with_size(mut n: size_t) -> *mut buffer_t {
    let mut self_0: *mut buffer_t =
        malloc(::std::mem::size_of::<buffer_t>() as libc::c_ulong) as *mut buffer_t;
    if self_0.is_null() {
        return 0 as *mut buffer_t;
    } else {
        (*self_0).len = n;
        (*self_0).alloc = calloc(n.wrapping_add(1i32 as libc::c_ulong), 1i32 as libc::c_ulong)
            as *mut libc::c_char;
        (*self_0).data = (*self_0).alloc;
        return self_0;
    };
}
#[no_mangle]
pub unsafe extern "C" fn buffer_new_with_string(mut str: *mut libc::c_char) -> *mut buffer_t {
    return buffer_new_with_string_length(str, strlen(str));
}
#[no_mangle]
pub unsafe extern "C" fn buffer_new_with_string_length(
    mut str: *mut libc::c_char,
    mut len: size_t,
) -> *mut buffer_t {
    let mut self_0: *mut buffer_t =
        malloc(::std::mem::size_of::<buffer_t>() as libc::c_ulong) as *mut buffer_t;
    if self_0.is_null() {
        return 0 as *mut buffer_t;
    } else {
        (*self_0).len = len;
        (*self_0).alloc = str;
        (*self_0).data = (*self_0).alloc;
        return self_0;
    };
}
#[no_mangle]
pub unsafe extern "C" fn buffer_new_with_copy(mut str: *mut libc::c_char) -> *mut buffer_t {
    let mut len: size_t = strlen(str);
    let mut self_0: *mut buffer_t = buffer_new_with_size(len);
    if self_0.is_null() {
        return 0 as *mut buffer_t;
    } else {
        memcpy(
            (*self_0).alloc as *mut libc::c_void,
            str as *const libc::c_void,
            len,
        );
        (*self_0).data = (*self_0).alloc;
        return self_0;
    };
}
#[no_mangle]
pub unsafe extern "C" fn buffer_size(mut self_0: *mut buffer_t) -> size_t {
    return (*self_0).len;
}
#[no_mangle]
pub unsafe extern "C" fn buffer_length(mut self_0: *mut buffer_t) -> size_t {
    return strlen((*self_0).data);
}
#[no_mangle]
pub unsafe extern "C" fn buffer_free(mut self_0: *mut buffer_t) -> () {
    free((*self_0).alloc as *mut libc::c_void);
    free(self_0 as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn buffer_prepend(
    mut self_0: *mut buffer_t,
    mut str: *mut libc::c_char,
) -> libc::c_int {
    let mut ret: libc::c_int = 0;
    let mut len: size_t = strlen(str);
    let mut prev: size_t = strlen((*self_0).data);
    let mut needed: size_t = len.wrapping_add(prev);
    // enough space
    if !((*self_0).len > needed) {
        // resize
        ret = buffer_resize(self_0, needed);
        if -1i32 == ret {
            return -1i32;
        }
    }
    // move
    memmove(
        (*self_0).data.offset(len as isize) as *mut libc::c_void,
        (*self_0).data as *const libc::c_void,
        len.wrapping_add(1i32 as libc::c_ulong),
    );
    memcpy(
        (*self_0).data as *mut libc::c_void,
        str as *const libc::c_void,
        len,
    );
    return 0i32;
}
/*
 * Resize to hold `n` bytes.
 */
#[no_mangle]
pub unsafe extern "C" fn buffer_resize(mut self_0: *mut buffer_t, mut n: size_t) -> libc::c_int {
    n = n.wrapping_add((1024i32 - 1i32) as libc::c_ulong) & !(1024i32 - 1i32) as libc::c_ulong;
    (*self_0).len = n;
    (*self_0).data = realloc(
        (*self_0).alloc as *mut libc::c_void,
        n.wrapping_add(1i32 as libc::c_ulong),
    ) as *mut libc::c_char;
    (*self_0).alloc = (*self_0).data;
    if (*self_0).alloc.is_null() {
        return -1i32;
    } else {
        *(*self_0).alloc.offset(n as isize) = '\u{0}' as i32 as libc::c_char;
        return 0i32;
    };
}
#[no_mangle]
pub unsafe extern "C" fn buffer_append(
    mut self_0: *mut buffer_t,
    mut str: *const libc::c_char,
) -> libc::c_int {
    return buffer_append_n(self_0, str, strlen(str));
}
#[no_mangle]
pub unsafe extern "C" fn buffer_append_n(
    mut self_0: *mut buffer_t,
    mut str: *const libc::c_char,
    mut len: size_t,
) -> libc::c_int {
    let mut prev: size_t = strlen((*self_0).data);
    let mut needed: size_t = len.wrapping_add(prev);
    // enough space
    if (*self_0).len > needed {
        strncat((*self_0).data, str, len);
        return 0i32;
    } else {
        // resize
        let mut ret: libc::c_int = buffer_resize(self_0, needed);
        if -1i32 == ret {
            return -1i32;
        } else {
            strncat((*self_0).data, str, len);
            return 0i32;
        }
    };
}
#[no_mangle]
pub unsafe extern "C" fn buffer_equals(
    mut self_0: *mut buffer_t,
    mut other: *mut buffer_t,
) -> libc::c_int {
    return (0i32 == strcmp((*self_0).data, (*other).data)) as libc::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn buffer_indexof(
    mut self_0: *mut buffer_t,
    mut str: *mut libc::c_char,
) -> ssize_t {
    let mut sub: *mut libc::c_char = strstr((*self_0).data, str);
    if sub.is_null() {
        return -1i32 as ssize_t;
    } else {
        return sub.wrapping_offset_from((*self_0).data) as libc::c_long;
    };
}
#[no_mangle]
pub unsafe extern "C" fn buffer_slice(
    mut buf: *mut buffer_t,
    mut from: size_t,
    mut to: ssize_t,
) -> *mut buffer_t {
    let mut len: size_t = strlen((*buf).data);
    // bad range
    if (to as libc::c_ulong) < from {
        return 0 as *mut buffer_t;
    } else {
        // relative to end
        if to < 0i32 as libc::c_long {
            to = len.wrapping_sub(!to as libc::c_ulong) as ssize_t
        }
        // cap end
        if to as libc::c_ulong > len {
            to = len as ssize_t
        }
        let mut n: size_t = (to as libc::c_ulong).wrapping_sub(from);
        let mut self_0: *mut buffer_t = buffer_new_with_size(n);
        memcpy(
            (*self_0).data as *mut libc::c_void,
            (*buf).data.offset(from as isize) as *const libc::c_void,
            n,
        );
        return self_0;
    };
}
#[no_mangle]
pub unsafe extern "C" fn buffer_compact(mut self_0: *mut buffer_t) -> ssize_t {
    let mut len: size_t = buffer_length(self_0);
    let mut rem: size_t = (*self_0).len.wrapping_sub(len);
    let mut buf: *mut libc::c_char = calloc(
        len.wrapping_add(1i32 as libc::c_ulong),
        1i32 as libc::c_ulong,
    ) as *mut libc::c_char;
    if buf.is_null() {
        return -1i32 as ssize_t;
    } else {
        memcpy(
            buf as *mut libc::c_void,
            (*self_0).data as *const libc::c_void,
            len,
        );
        free((*self_0).alloc as *mut libc::c_void);
        (*self_0).len = len;
        (*self_0).alloc = buf;
        (*self_0).data = (*self_0).alloc;
        return rem as ssize_t;
    };
}
#[no_mangle]
pub unsafe extern "C" fn buffer_fill(mut self_0: *mut buffer_t, mut c: libc::c_int) -> () {
    memset((*self_0).data as *mut libc::c_void, c, (*self_0).len);
}
#[no_mangle]
pub unsafe extern "C" fn buffer_clear(mut self_0: *mut buffer_t) -> () {
    buffer_fill(self_0, 0i32);
}
#[no_mangle]
pub unsafe extern "C" fn buffer_trim_left(mut self_0: *mut buffer_t) -> () {
    let mut c: libc::c_int = 0;
    loop {
        c = *(*self_0).data as libc::c_int;
        if !(0 != c
            && 0 != *(*__ctype_b_loc()).offset(c as isize) as libc::c_int
                & _ISspace as libc::c_int as libc::c_ushort as libc::c_int)
        {
            break;
        }
        (*self_0).data = (*self_0).data.offset(1isize)
    }
}
#[no_mangle]
pub unsafe extern "C" fn buffer_trim_right(mut self_0: *mut buffer_t) -> () {
    let mut c: libc::c_int = 0;
    let mut i: size_t = buffer_length(self_0).wrapping_sub(1i32 as libc::c_ulong);
    loop {
        c = *(*self_0).data.offset(i as isize) as libc::c_int;
        if !(0 != c
            && 0 != *(*__ctype_b_loc()).offset(c as isize) as libc::c_int
                & _ISspace as libc::c_int as libc::c_ushort as libc::c_int)
        {
            break;
        }
        let fresh0 = i;
        i = i.wrapping_sub(1);
        *(*self_0).data.offset(fresh0 as isize) = 0i32 as libc::c_char
    }
}
#[no_mangle]
pub unsafe extern "C" fn buffer_trim(mut self_0: *mut buffer_t) -> () {
    buffer_trim_left(self_0);
    buffer_trim_right(self_0);
}
#[no_mangle]
pub unsafe extern "C" fn buffer_print(mut self_0: *mut buffer_t) -> () {
    let mut i: libc::c_int = 0;
    let mut len: size_t = (*self_0).len;
    printf(b"\n \x00" as *const u8 as *const libc::c_char);
    // hex
    i = 0i32;
    while (i as libc::c_ulong) < len {
        printf(
            b" %02x\x00" as *const u8 as *const libc::c_char,
            *(*self_0).alloc.offset(i as isize) as libc::c_int,
        );
        if (i + 1i32) % 8i32 == 0i32 {
            printf(b"\n \x00" as *const u8 as *const libc::c_char);
        }
        i += 1
    }
    printf(b"\n\x00" as *const u8 as *const libc::c_char);
}
