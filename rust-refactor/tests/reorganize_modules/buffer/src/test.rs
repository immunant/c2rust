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
    extern "C" {
        // prototypes
        #[no_mangle]
        pub fn buffer_new() -> *mut buffer_t;
        #[no_mangle]
        pub fn buffer_new_with_size(n: size_t) -> *mut buffer_t;
        #[no_mangle]
        pub fn buffer_new_with_copy(str: *mut libc::c_char) -> *mut buffer_t;
        #[no_mangle]
        pub fn buffer_size(self_0: *mut buffer_t) -> size_t;
        #[no_mangle]
        pub fn buffer_length(self_0: *mut buffer_t) -> size_t;
        #[no_mangle]
        pub fn buffer_free(self_0: *mut buffer_t) -> ();
        #[no_mangle]
        pub fn buffer_prepend(self_0: *mut buffer_t, str: *mut libc::c_char) -> libc::c_int;
        #[no_mangle]
        pub fn buffer_append(self_0: *mut buffer_t, str: *const libc::c_char) -> libc::c_int;
        #[no_mangle]
        pub fn buffer_append_n(
            self_0: *mut buffer_t,
            str: *const libc::c_char,
            len: size_t,
        ) -> libc::c_int;
        #[no_mangle]
        pub fn buffer_equals(self_0: *mut buffer_t, other: *mut buffer_t) -> libc::c_int;
        #[no_mangle]
        pub fn buffer_indexof(self_0: *mut buffer_t, str: *mut libc::c_char) -> ssize_t;
        #[no_mangle]
        pub fn buffer_slice(self_0: *mut buffer_t, from: size_t, to: ssize_t) -> *mut buffer_t;
        #[no_mangle]
        pub fn buffer_compact(self_0: *mut buffer_t) -> ssize_t;
        #[no_mangle]
        pub fn buffer_fill(self_0: *mut buffer_t, c: libc::c_int) -> ();
        #[no_mangle]
        pub fn buffer_clear(self_0: *mut buffer_t) -> ();
        #[no_mangle]
        pub fn buffer_trim_left(self_0: *mut buffer_t) -> ();
        #[no_mangle]
        pub fn buffer_trim_right(self_0: *mut buffer_t) -> ();
        #[no_mangle]
        pub fn buffer_trim(self_0: *mut buffer_t) -> ();
    }
}
#[cfg(not(source_header = "/usr/include/assert.h"))]
pub mod assert_h {
    use super::libc;
    extern "C" {
        #[no_mangle]
        pub fn __assert_fail(
            __assertion: *const libc::c_char,
            __file: *const libc::c_char,
            __line: libc::c_uint,
            __function: *const libc::c_char,
        ) -> !;
    }
}
#[cfg(not(source_header = "/usr/include/stdlib.h"))]
pub mod stdlib_h {
    use super::libc;
    extern "C" {
        #[no_mangle]
        pub fn exit(_: libc::c_int) -> !;
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
#[cfg(not(source_header = "/usr/include/string.h"))]
pub mod string_h {
    use super::libc;
    extern "C" {
        #[no_mangle]
        pub fn strcmp(_: *const libc::c_char, _: *const libc::c_char) -> libc::c_int;
        #[no_mangle]
        pub fn strlen(_: *const libc::c_char) -> libc::c_ulong;
    }
}
use self::assert_h::__assert_fail;
use self::buffer_h::{
    buffer_append, buffer_append_n, buffer_clear, buffer_compact, buffer_equals, buffer_fill,
    buffer_free, buffer_indexof, buffer_length, buffer_new, buffer_new_with_copy,
    buffer_new_with_size, buffer_prepend, buffer_size, buffer_slice, buffer_t, buffer_trim,
    buffer_trim_left, buffer_trim_right,
};
use self::stddef_h::size_t;
use self::stdio_h::printf;
use self::stdlib_h::exit;
use self::string_h::{strcmp, strlen};
use self::sys_types_h::ssize_t;
use self::types_h::__ssize_t;
//
// test.c
//
// Copyright (c) 2012 TJ Holowaychuk <tj@vision-media.ca>
//
#[no_mangle]
pub unsafe extern "C" fn equal(mut a: *mut libc::c_char, mut b: *mut libc::c_char) -> () {
    if 0 != strcmp(a, b) {
        printf(b"\n\x00" as *const u8 as *const libc::c_char);
        printf(
            b"  expected: \'%s\'\n\x00" as *const u8 as *const libc::c_char,
            a,
        );
        printf(
            b"    actual: \'%s\'\n\x00" as *const u8 as *const libc::c_char,
            b,
        );
        printf(b"\n\x00" as *const u8 as *const libc::c_char);
        exit(1i32);
    } else {
        return;
    };
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_new() -> () {
    let mut buf: *mut buffer_t = buffer_new();
    if 64i32 as libc::c_ulong == buffer_size(buf) {
    } else {
        __assert_fail(
            b"BUFFER_DEFAULT_SIZE == buffer_size(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            28i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"void test_buffer_new()\x00",
            )).as_ptr(),
        );
    };
    if 0i32 as libc::c_ulong == buffer_length(buf) {
    } else {
        __assert_fail(
            b"0 == buffer_length(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            29i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"void test_buffer_new()\x00",
            )).as_ptr(),
        );
    };
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_new_with_size() -> () {
    let mut buf: *mut buffer_t = buffer_new_with_size(1024i32 as size_t);
    if 1024i32 as libc::c_ulong == buffer_size(buf) {
    } else {
        __assert_fail(
            b"1024 == buffer_size(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            36i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 33], &[libc::c_char; 33]>(
                b"void test_buffer_new_with_size()\x00",
            )).as_ptr(),
        );
    };
    if 0i32 as libc::c_ulong == buffer_length(buf) {
    } else {
        __assert_fail(
            b"0 == buffer_length(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            37i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 33], &[libc::c_char; 33]>(
                b"void test_buffer_new_with_size()\x00",
            )).as_ptr(),
        );
    };
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_append() -> () {
    let mut buf: *mut buffer_t = buffer_new();
    if 0i32 == buffer_append(buf, b"Hello\x00" as *const u8 as *const libc::c_char) {
    } else {
        __assert_fail(
            b"0 == buffer_append(buf, \"Hello\")\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            44i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 26], &[libc::c_char; 26]>(
                b"void test_buffer_append()\x00",
            )).as_ptr(),
        );
    };
    if 0i32 == buffer_append(buf, b" World\x00" as *const u8 as *const libc::c_char) {
    } else {
        __assert_fail(
            b"0 == buffer_append(buf, \" World\")\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            45i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 26], &[libc::c_char; 26]>(
                b"void test_buffer_append()\x00",
            )).as_ptr(),
        );
    };
    if strlen(b"Hello World\x00" as *const u8 as *const libc::c_char) == buffer_length(buf) {
    } else {
        __assert_fail(
            b"strlen(\"Hello World\") == buffer_length(buf)\x00" as *const u8
                as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            46i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 26], &[libc::c_char; 26]>(
                b"void test_buffer_append()\x00",
            )).as_ptr(),
        );
    };
    equal(
        b"Hello World\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*buf).data,
    );
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_append_n() -> () {
    let mut buf: *mut buffer_t = buffer_new();
    if 0i32 == buffer_append_n(
        buf,
        b"subway\x00" as *const u8 as *const libc::c_char,
        3i32 as size_t,
    ) {
    } else {
        __assert_fail(
            b"0 == buffer_append_n(buf, \"subway\", 3)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            54i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 28], &[libc::c_char; 28]>(
                b"void test_buffer_append_n()\x00",
            )).as_ptr(),
        );
    };
    if 0i32 == buffer_append_n(
        buf,
        b"marines\x00" as *const u8 as *const libc::c_char,
        6i32 as size_t,
    ) {
    } else {
        __assert_fail(
            b"0 == buffer_append_n(buf, \"marines\", 6)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            55i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 28], &[libc::c_char; 28]>(
                b"void test_buffer_append_n()\x00",
            )).as_ptr(),
        );
    };
    if strlen(b"submarine\x00" as *const u8 as *const libc::c_char) == buffer_length(buf) {
    } else {
        __assert_fail(
            b"strlen(\"submarine\") == buffer_length(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            56i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 28], &[libc::c_char; 28]>(
                b"void test_buffer_append_n()\x00",
            )).as_ptr(),
        );
    };
    equal(
        b"submarine\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*buf).data,
    );
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_append__grow() -> () {
    let mut buf: *mut buffer_t = buffer_new_with_size(10i32 as size_t);
    if 0i32 == buffer_append(buf, b"Hello\x00" as *const u8 as *const libc::c_char) {
    } else {
        __assert_fail(
            b"0 == buffer_append(buf, \"Hello\")\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            64i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 32], &[libc::c_char; 32]>(
                b"void test_buffer_append__grow()\x00",
            )).as_ptr(),
        );
    };
    if 0i32 == buffer_append(buf, b" tobi\x00" as *const u8 as *const libc::c_char) {
    } else {
        __assert_fail(
            b"0 == buffer_append(buf, \" tobi\")\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            65i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 32], &[libc::c_char; 32]>(
                b"void test_buffer_append__grow()\x00",
            )).as_ptr(),
        );
    };
    if 0i32 == buffer_append(buf, b" was\x00" as *const u8 as *const libc::c_char) {
    } else {
        __assert_fail(
            b"0 == buffer_append(buf, \" was\")\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            66i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 32], &[libc::c_char; 32]>(
                b"void test_buffer_append__grow()\x00",
            )).as_ptr(),
        );
    };
    if 0i32 == buffer_append(buf, b" here\x00" as *const u8 as *const libc::c_char) {
    } else {
        __assert_fail(
            b"0 == buffer_append(buf, \" here\")\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            67i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 32], &[libc::c_char; 32]>(
                b"void test_buffer_append__grow()\x00",
            )).as_ptr(),
        );
    };
    let mut str: *mut libc::c_char =
        b"Hello tobi was here\x00" as *const u8 as *const libc::c_char as *mut libc::c_char;
    equal(str, (*buf).data);
    if 1024i32 as libc::c_ulong == buffer_size(buf) {
    } else {
        __assert_fail(
            b"1024 == buffer_size(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            71i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 32], &[libc::c_char; 32]>(
                b"void test_buffer_append__grow()\x00",
            )).as_ptr(),
        );
    };
    if strlen(str) == buffer_length(buf) {
    } else {
        __assert_fail(
            b"strlen(str) == buffer_length(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            72i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 32], &[libc::c_char; 32]>(
                b"void test_buffer_append__grow()\x00",
            )).as_ptr(),
        );
    };
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_prepend() -> () {
    let mut buf: *mut buffer_t = buffer_new();
    if 0i32 == buffer_append(buf, b" World\x00" as *const u8 as *const libc::c_char) {
    } else {
        __assert_fail(
            b"0 == buffer_append(buf, \" World\")\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            79i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"void test_buffer_prepend()\x00",
            )).as_ptr(),
        );
    };
    if 0i32 == buffer_prepend(
        buf,
        b"Hello\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    ) {
    } else {
        __assert_fail(
            b"0 == buffer_prepend(buf, \"Hello\")\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            80i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"void test_buffer_prepend()\x00",
            )).as_ptr(),
        );
    };
    if strlen(b"Hello World\x00" as *const u8 as *const libc::c_char) == buffer_length(buf) {
    } else {
        __assert_fail(
            b"strlen(\"Hello World\") == buffer_length(buf)\x00" as *const u8
                as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            81i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"void test_buffer_prepend()\x00",
            )).as_ptr(),
        );
    };
    equal(
        b"Hello World\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*buf).data,
    );
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_slice() -> () {
    let mut buf: *mut buffer_t = buffer_new();
    buffer_append(buf, b"Tobi Ferret\x00" as *const u8 as *const libc::c_char);
    let mut a: *mut buffer_t = buffer_slice(buf, 2i32 as size_t, 8i32 as ssize_t);
    equal(
        b"Tobi Ferret\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*buf).data,
    );
    equal(
        b"bi Fer\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*a).data,
    );
    buffer_free(buf);
    buffer_free(a);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_slice__range_error() -> () {
    let mut buf: *mut buffer_t = buffer_new_with_copy(
        b"Tobi Ferret\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    let mut a: *mut buffer_t = buffer_slice(buf, 10i32 as size_t, 2i32 as ssize_t);
    if a.is_null() {
    } else {
        __assert_fail(
            b"NULL == a\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            103i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 38], &[libc::c_char; 38]>(
                b"void test_buffer_slice__range_error()\x00",
            )).as_ptr(),
        );
    };
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_slice__end() -> () {
    let mut buf: *mut buffer_t = buffer_new_with_copy(
        b"Tobi Ferret\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    let mut a: *mut buffer_t = buffer_slice(buf, 5i32 as size_t, -1i32 as ssize_t);
    equal(
        b"Tobi Ferret\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*buf).data,
    );
    equal(
        b"Ferret\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*a).data,
    );
    let mut b: *mut buffer_t = buffer_slice(buf, 5i32 as size_t, -3i32 as ssize_t);
    equal(
        b"Ferr\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*b).data,
    );
    let mut c: *mut buffer_t = buffer_slice(buf, 8i32 as size_t, -1i32 as ssize_t);
    equal(
        b"ret\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*c).data,
    );
    buffer_free(buf);
    buffer_free(a);
    buffer_free(b);
    buffer_free(c);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_slice__end_overflow() -> () {
    let mut buf: *mut buffer_t = buffer_new_with_copy(
        b"Tobi Ferret\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    let mut a: *mut buffer_t = buffer_slice(buf, 5i32 as size_t, 1000i32 as ssize_t);
    equal(
        b"Tobi Ferret\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*buf).data,
    );
    equal(
        b"Ferret\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*a).data,
    );
    buffer_free(a);
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_equals() -> () {
    let mut a: *mut buffer_t =
        buffer_new_with_copy(b"Hello\x00" as *const u8 as *const libc::c_char as *mut libc::c_char);
    let mut b: *mut buffer_t =
        buffer_new_with_copy(b"Hello\x00" as *const u8 as *const libc::c_char as *mut libc::c_char);
    if 1i32 == buffer_equals(a, b) {
    } else {
        __assert_fail(
            b"1 == buffer_equals(a, b)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            142i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 26], &[libc::c_char; 26]>(
                b"void test_buffer_equals()\x00",
            )).as_ptr(),
        );
    };
    buffer_append(b, b" World\x00" as *const u8 as *const libc::c_char);
    if 0i32 == buffer_equals(a, b) {
    } else {
        __assert_fail(
            b"0 == buffer_equals(a, b)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            145i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 26], &[libc::c_char; 26]>(
                b"void test_buffer_equals()\x00",
            )).as_ptr(),
        );
    };
    buffer_free(a);
    buffer_free(b);
}
/*
void test_buffer_formatting() {
  buffer_t *buf = buffer_new();
  int result = buffer_appendf(buf, "%d %s", 3, "cow");
  assert(0 == result);
  equal("3 cow", buffer_string(buf));
  result = buffer_appendf(buf, " - 0x%08X", 0xdeadbeef);
  assert(0 == result);
  equal("3 cow - 0xDEADBEEF", buffer_string(buf));
  buffer_free(buf);
}
*/
#[no_mangle]
pub unsafe extern "C" fn test_buffer_indexof() -> () {
    let mut buf: *mut buffer_t = buffer_new_with_copy(
        b"Tobi is a ferret\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    let mut i: ssize_t = buffer_indexof(
        buf,
        b"is\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if 5i32 as libc::c_long == i {
    } else {
        __assert_fail(
            b"5 == i\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            169i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"void test_buffer_indexof()\x00",
            )).as_ptr(),
        );
    };
    i = buffer_indexof(
        buf,
        b"a\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if 8i32 as libc::c_long == i {
    } else {
        __assert_fail(
            b"8 == i\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            172i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"void test_buffer_indexof()\x00",
            )).as_ptr(),
        );
    };
    i = buffer_indexof(
        buf,
        b"something\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if -1i32 as libc::c_long == i {
    } else {
        __assert_fail(
            b"-1 == i\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            175i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"void test_buffer_indexof()\x00",
            )).as_ptr(),
        );
    };
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_fill() -> () {
    let mut buf: *mut buffer_t =
        buffer_new_with_copy(b"Hello\x00" as *const u8 as *const libc::c_char as *mut libc::c_char);
    if 5i32 as libc::c_ulong == buffer_length(buf) {
    } else {
        __assert_fail(
            b"5 == buffer_length(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            183i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 24], &[libc::c_char; 24]>(
                b"void test_buffer_fill()\x00",
            )).as_ptr(),
        );
    };
    buffer_fill(buf, 0i32);
    if 0i32 as libc::c_ulong == buffer_length(buf) {
    } else {
        __assert_fail(
            b"0 == buffer_length(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            186i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 24], &[libc::c_char; 24]>(
                b"void test_buffer_fill()\x00",
            )).as_ptr(),
        );
    };
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_clear() -> () {
    let mut buf: *mut buffer_t =
        buffer_new_with_copy(b"Hello\x00" as *const u8 as *const libc::c_char as *mut libc::c_char);
    if 5i32 as libc::c_ulong == buffer_length(buf) {
    } else {
        __assert_fail(
            b"5 == buffer_length(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            193i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 25], &[libc::c_char; 25]>(
                b"void test_buffer_clear()\x00",
            )).as_ptr(),
        );
    };
    buffer_clear(buf);
    if 0i32 as libc::c_ulong == buffer_length(buf) {
    } else {
        __assert_fail(
            b"0 == buffer_length(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            196i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 25], &[libc::c_char; 25]>(
                b"void test_buffer_clear()\x00",
            )).as_ptr(),
        );
    };
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_trim() -> () {
    let mut buf: *mut buffer_t = buffer_new_with_copy(
        b"  Hello\n\n \x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    buffer_trim(buf);
    equal(
        b"Hello\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*buf).data,
    );
    buffer_free(buf);
    buf = buffer_new_with_copy(
        b"  Hello\n\n \x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    buffer_trim_left(buf);
    equal(
        b"Hello\n\n \x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*buf).data,
    );
    buffer_free(buf);
    buf = buffer_new_with_copy(
        b"  Hello\n\n \x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    buffer_trim_right(buf);
    equal(
        b"  Hello\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*buf).data,
    );
    buffer_free(buf);
}
#[no_mangle]
pub unsafe extern "C" fn test_buffer_compact() -> () {
    let mut buf: *mut buffer_t = buffer_new_with_copy(
        b"  Hello\n\n \x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    buffer_trim(buf);
    if 5i32 as libc::c_ulong == buffer_length(buf) {
    } else {
        __assert_fail(
            b"5 == buffer_length(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            222i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"void test_buffer_compact()\x00",
            )).as_ptr(),
        );
    };
    if 10i32 as libc::c_ulong == buffer_size(buf) {
    } else {
        __assert_fail(
            b"10 == buffer_size(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            223i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"void test_buffer_compact()\x00",
            )).as_ptr(),
        );
    };
    let mut removed: ssize_t = buffer_compact(buf);
    if 5i32 as libc::c_long == removed {
    } else {
        __assert_fail(
            b"5 == removed\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            226i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"void test_buffer_compact()\x00",
            )).as_ptr(),
        );
    };
    if 5i32 as libc::c_ulong == buffer_length(buf) {
    } else {
        __assert_fail(
            b"5 == buffer_length(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            227i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"void test_buffer_compact()\x00",
            )).as_ptr(),
        );
    };
    if 5i32 as libc::c_ulong == buffer_size(buf) {
    } else {
        __assert_fail(
            b"5 == buffer_size(buf)\x00" as *const u8 as *const libc::c_char,
            b"test.c\x00" as *const u8 as *const libc::c_char,
            228i32 as libc::c_uint,
            (*::std::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"void test_buffer_compact()\x00",
            )).as_ptr(),
        );
    };
    equal(
        b"Hello\x00" as *const u8 as *const libc::c_char as *mut libc::c_char,
        (*buf).data,
    );
    buffer_free(buf);
}
unsafe fn main_0() -> libc::c_int {
    test_buffer_new();
    test_buffer_new_with_size();
    test_buffer_append();
    test_buffer_append__grow();
    test_buffer_append_n();
    test_buffer_prepend();
    test_buffer_slice();
    test_buffer_slice__range_error();
    test_buffer_slice__end();
    test_buffer_slice__end_overflow();
    test_buffer_equals();
    /*test_buffer_formatting();*/
    test_buffer_indexof();
    test_buffer_fill();
    test_buffer_clear();
    test_buffer_trim();
    test_buffer_compact();
    printf(
        b"\n  \x1b[32m\xe2\x9c\x93 \x1b[90mok\x1b[0m\n\n\x00" as *const u8 as *const libc::c_char,
    );
    return 0i32;
}
pub fn main() -> () {
    unsafe { ::std::process::exit(main_0() as i32) }
}
