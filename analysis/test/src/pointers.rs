#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut,
    unused_variables,
    unused_parens
)]
extern "C" {
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(__ptr: *mut libc::c_void);
}

#[cfg(not(feature = "miri"))]
extern "C" {
    fn printf(_: *const libc::c_char, _: ...) -> libc::c_int;
}

/// `miri` does not support calling variadic functions like [`printf`],
/// but we want to test for UB, leaks, etc. using `cargo miri run`.
///
/// Luckily, all [`printf`] calls in this module are monomorphic,
/// in that they all have the same format string and same call signature,
/// so we can replace it with a [`printf`] shim that preserves the behavior
/// only for the exact monomorphic usages in this module.
///
/// Note that there is no way to detect `miri` is running,
/// so we have to put this under a separate `miri` feature
/// that should be enabled when testing under `miri` with
/// `cargo miri run --features miri`.
#[cfg(feature = "miri")]
fn printf(fmt: *const libc::c_char, i: i32) -> libc::c_int {
    use std::ffi::CStr;
    assert_eq!(
        unsafe { CStr::from_ptr(fmt) },
        CStr::from_bytes_with_nul(b"%i\n\x00").unwrap()
    );
    let s = format!("{i}\n");
    print!("{s}");
    s.len() as libc::c_int
}

/// Hidden from instrumentation so that we can polyfill [`reallocarray`] with it.
const REALLOC: unsafe extern "C" fn(*mut libc::c_void, libc::c_ulong) -> *mut libc::c_void =
    realloc;

/// Polyfill [`reallocarray`] as macOS does not have [`reallocarray`].
///
/// Normally we'd only polyfill it on macOS, but then we'd need a different snapshot file for macOS,
/// as polyfilling results in a couple of extra copies.
/// Thus, we just polyfill always.
#[no_mangle]
unsafe fn reallocarray(ptr: *mut libc::c_void, nmemb: size_t, size: size_t) -> *mut libc::c_void {
    REALLOC(ptr, nmemb * size)
}

use libc::*;
use std::mem;
pub type size_t = libc::c_ulong;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct T {
    pub field: libc::c_int,
    pub field2: libc::c_ulong,
    pub field3: *const S,
    pub field4: libc::c_int,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct S {
    pub field: libc::c_int,
    pub field2: libc::c_ulong,
    pub field3: *const S,
    pub field4: T,
}
#[no_mangle]
pub static mut global: *mut S = 0 as *const S as *mut S;
#[no_mangle]
pub unsafe extern "C" fn malloc_wrapper(mut size: size_t) -> *mut libc::c_void {
    return malloc(size);
}
#[no_mangle]
pub unsafe extern "C" fn recur(x: libc::c_int, s: *mut S) {
    if x == 0 {
        return free(s as *mut libc::c_void);
    }

    recur(x - 1, s);
    let y = s;
}
#[no_mangle]
pub unsafe extern "C" fn simple() {
    let mut x = malloc(mem::size_of::<S>() as c_ulong) as *mut S;
    let mut x2 = x;
    let y = malloc(mem::size_of::<S>() as c_ulong) as *mut S;
    let z = std::ptr::addr_of!((*x).field);
    x = y;
    (*x).field = 10i32;
    (*y).field = (*x).field;
    (*x).field2 = 9u64;
    let k = (*x).field;
    let z = std::ptr::addr_of!((*x).field2);
    (*x).field3 = std::ptr::addr_of!(*x) as *const S;
    (*y).field4 = T {
        field: 0i32,
        field2: 0u64,
        field3: 0 as *const S,
        field4: 0i32,
    };
    let s = *y;
    *x = s;
    recur(3, x);
    free(x2 as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn simple1() {
    let mut x = malloc(mem::size_of::<S>() as c_ulong) as *mut S;
    let z = realloc(x as *mut libc::c_void, mem::size_of::<S>() as c_ulong) as *mut S;

    let x_copy = z;
    (*x_copy).field = 10i32;
    let x_copy_2 = z;
    let x_copy_copy = x_copy;
    let addr_of_copy = std::ptr::addr_of!(x_copy_copy);
    let i_cast = x as usize;
    let x_from_int = i_cast as *const libc::c_void;
    free(z as *mut libc::c_void);
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct connection {
    pub fd: libc::c_int,
    pub fdn: *mut fdnode,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct server {
    pub ev: *mut fdevents,
}
pub struct fdevents {
    pub fdarray: *mut *mut fdnode,
}

pub type handler_t = libc::c_uint;
pub type fdevent_handler =
    Option<unsafe extern "C" fn(*mut libc::c_void, libc::c_int) -> handler_t>;
unsafe extern "C" fn connection_handle_fdevent(
    context: *mut libc::c_void,
    revents: libc::c_int,
) -> handler_t {
    return 1;
}
pub struct fdnode_st {
    pub handler: fdevent_handler,
    pub ctx: *mut libc::c_void,
    pub fd: libc::c_int,
    pub events: libc::c_int,
    pub fde_ndx: libc::c_int,
}
pub type fdnode = fdnode_st;

unsafe extern "C" fn fdnode_init() -> *mut fdnode {
    let fdn: *mut fdnode = calloc(
        1 as libc::c_int as libc::c_ulong,
        ::std::mem::size_of::<fdnode>() as libc::c_ulong,
    ) as *mut fdnode;
    if fdn.is_null() {
        println!("It's null");
    }
    return fdn;
}
#[no_mangle]
pub unsafe extern "C" fn fdevent_register(
    mut ev: *mut fdevents,
    mut fd: libc::c_int,
    mut handler: fdevent_handler,
    mut ctx: *mut libc::c_void,
) -> *mut fdnode {
    let ref mut fresh0 = *((*ev).fdarray).offset(fd as isize);
    *fresh0 = fdnode_init();
    let mut fdn: *mut fdnode = *fresh0;
    (*fdn).handler = handler;
    (*fdn).fd = fd;
    (*fdn).ctx = ctx;
    (*fdn).events = 0 as libc::c_int;
    (*fdn).fde_ndx = -(1 as libc::c_int);
    return fdn;
}
#[no_mangle]
pub unsafe extern "C" fn connection_accepted(
    mut srv: *mut server,
    mut cnt: libc::c_int,
) -> *mut connection {
    let con = malloc(::std::mem::size_of::<connection>() as libc::c_ulong) as *mut connection;
    (*con).fd = cnt;
    (*con).fdn = fdevent_register(
        (*srv).ev,
        (*con).fd,
        Some(
            connection_handle_fdevent
                as unsafe extern "C" fn(*mut libc::c_void, libc::c_int) -> handler_t,
        ),
        con as *mut libc::c_void,
    );
    return con;
}
unsafe extern "C" fn connection_close(mut srv: *mut server, mut con: *mut connection) {
    fdevent_fdnode_event_del((*srv).ev, (*con).fdn);
    fdevent_unregister((*srv).ev, (*con).fd);
    free(con as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn fdevent_fdnode_event_del(mut ev: *mut fdevents, mut fdn: *mut fdnode) {
    if !fdn.is_null() {
        fdevent_fdnode_event_unsetter(ev, fdn);
    }
}
unsafe extern "C" fn fdevent_fdnode_event_unsetter(mut ev: *mut fdevents, mut fdn: *mut fdnode) {
    if -(1 as libc::c_int) == (*fdn).fde_ndx {
        return;
    }
    (*fdn).fde_ndx = -(1 as libc::c_int);
    (*fdn).events = 0 as libc::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn fdevent_unregister(mut ev: *mut fdevents, mut fd: libc::c_int) {
    let mut fdn: *mut fdnode = *((*ev).fdarray).offset(fd as isize);
    if fdn as uintptr_t & 0x3 as libc::c_int as usize != 0 as uintptr_t {
        return;
    }
    let ref mut fresh1 = *((*ev).fdarray).offset(fd as isize);
    *fresh1 = 0 as *mut fdnode;
    fdnode_free(fdn);
}
unsafe extern "C" fn fdnode_free(mut fdn: *mut fdnode) {
    free(fdn as *mut libc::c_void);
}
pub unsafe extern "C" fn lighttpd_test() {
    let fdarr = malloc(::std::mem::size_of::<*mut fdnode>() as libc::c_ulong) as *mut *mut fdnode;
    let fdes = malloc(::std::mem::size_of::<fdevents>() as libc::c_ulong) as *mut fdevents;
    (*fdes).fdarray = fdarr;
    let ref mut srvr = server { ev: fdes };
    let connection = connection_accepted(srvr as *mut server, 0);
    connection_close(srvr, connection);
    free(fdarr as *mut libc::c_void);
    free(fdes as *mut libc::c_void);
}

#[no_mangle]
pub unsafe extern "C" fn exercise_allocator() {
    let mut s: *mut S = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    s = realloc(
        s as *mut libc::c_void,
        (2i32 as libc::c_ulong).wrapping_mul(::std::mem::size_of::<S>() as libc::c_ulong),
    ) as *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    let mut i: libc::c_int = 0i32;
    while i < 2i32 {
        printf(
            b"%i\n\x00" as *const u8 as *const libc::c_char,
            (*s.offset(i as isize)).field,
        );
        i += 1
    }
    s = reallocarray(
        s as *mut libc::c_void,
        3i32 as size_t,
        ::std::mem::size_of::<S>() as libc::c_ulong,
    ) as *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    (*s.offset(2isize)).field = 12i32;
    let mut i_0: libc::c_int = 0i32;
    while i_0 < 3i32 {
        printf(
            b"%i\n\x00" as *const u8 as *const libc::c_char,
            (*s.offset(i_0 as isize)).field,
        );
        i_0 += 1
    }
    free(s as *mut libc::c_void);
    s = calloc(
        4i32 as libc::c_ulong,
        ::std::mem::size_of::<S>() as libc::c_ulong,
    ) as *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    (*s.offset(2isize)).field = 12i32;
    (*s.offset(3isize)).field = 13i32;
    let mut i_1: libc::c_int = 0i32;
    while i_1 < 4i32 {
        printf(
            b"%i\n\x00" as *const u8 as *const libc::c_char,
            (*s.offset(i_1 as isize)).field,
        );
        i_1 += 1
    }
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn simple_analysis() {
    let mut s: *mut S = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn inter_function_analysis() {
    let mut s: *mut S = malloc_wrapper(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 11i32;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn analysis2_helper(mut s: *mut S) {
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
}
#[no_mangle]
pub unsafe extern "C" fn analysis2() {
    let mut s: *mut S = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    analysis2_helper(s);
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn no_owner(mut should_free: libc::c_int) {
    global = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    if 0 != should_free {
        free(global as *mut libc::c_void);
    };
}
#[no_mangle]
pub unsafe extern "C" fn invalid() {
    let mut s: *mut S = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    global = s;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    printf(
        b"%i\n\x00" as *const u8 as *const libc::c_char,
        (*global).field,
    );
    global = 0 as *mut S;
    free(s as *mut libc::c_void);
}
pub unsafe extern "C" fn testing() {
    let mut x = 10i32;
    let mut y = 32i32;
    let mut ptr = &mut x as *mut i32;
    let ref mut fresh1 = ptr;
    *fresh1 = &mut x as *mut i32;
}
#[no_mangle]
pub unsafe extern "C" fn test_malloc_free() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    free(s);
}
#[no_mangle]
pub unsafe extern "C" fn test_malloc_free_cast() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn foo(bar: *mut libc::c_void) {
    let baz = bar;
}
#[no_mangle]
pub unsafe extern "C" fn test_arg() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    foo(s);
    let t = s;
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn foo_rec(n: i32, bar: *mut libc::c_void) -> *mut libc::c_void {
    if n != 0 {
        let x = foo_rec(n - 1, bar);
        let baz = x;
        return baz;
    }

    bar
}
#[no_mangle]
pub unsafe extern "C" fn test_arg_rec() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let t = foo_rec(3, s);
    free(s as *mut libc::c_void);
}
pub fn shared_ref_foo(x: &u8) -> &u8 {
    x
}
#[no_mangle]
pub unsafe extern "C" fn test_addr_taken_arg(mut t: T) {
    t.field3 = 0 as *const S;
    let z = &t;
}
#[no_mangle]
pub unsafe extern "C" fn test_shared_ref() {
    let x = 2;
    let y = &x;
    let z = y;
    let foo = shared_ref_foo(z);
    let bar = std::ptr::addr_of!(*foo);
}
#[no_mangle]
pub unsafe extern "C" fn test_unique_ref() {
    let mut x = 10i32;
    let mut y = 32i32;
    let mut ptr = &mut x as *mut i32;
    let ref mut fresh1 = ptr;
    *fresh1 = &mut x as *mut i32;
}
#[no_mangle]
pub unsafe extern "C" fn test_ref_field() {
    let t = T {
        field: 0i32,
        field2: 0u64,
        field3: 0 as *const S,
        field4: 0i32,
    };

    let ref mut s = S {
        field: 0i32,
        field2: 0u64,
        field3: 0 as *const S,
        field4: t,
    };
    s.field4.field4 = s.field4.field4;
}
#[no_mangle]
pub unsafe extern "C" fn test_addr_taken() {
    let x = 2;
    let y = 2 + x;
    let px = std::ptr::addr_of!(x);
    let z = x + y;
}
#[no_mangle]
pub fn test_addr_taken_cond(cond: bool) {
    // let x = if cond { ... } else { ... }; ... &x ...
    let x = if cond { 1 } else { 2 };

    let y = &x;
    let a = x;
}
#[no_mangle]
pub fn test_addr_taken_init_cond(cond: bool) {
    // let mut x; if cond { x = 1; ... &x ... }; x = 2; ... &x ...
    let mut x;
    let mut y;
    if cond {
        x = 1;
        y = &x;
    }
    x = 2;
    let z = x;
}
#[no_mangle]
pub unsafe fn test_addr_taken_loop() {
    // loop { let x = ...; ... &x ... }
    let mut count = 0;
    loop {
        let x = 2;
        let z = if count % 2 == 0 { &x } else { &1 };
        if count >= 3 {
            break;
        }
        count += *z;
    }
}
#[no_mangle]
pub unsafe extern "C" fn test_realloc_reassign() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    s = realloc(s, 2 * mem::size_of::<S>() as c_ulong);
    free(s);
}
#[no_mangle]
pub unsafe extern "C" fn test_realloc_fresh() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let p = realloc(s, mem::size_of::<S>() as c_ulong);
    free(p);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_addr() {
    let s = calloc(1, ::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    let x = (*s);
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_overwrite() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let s2 = s;
    let t = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    s = t;
    free(s);
    free(s2);
}
#[no_mangle]
pub unsafe extern "C" fn test_store_addr() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_other_store_self() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    let t = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    (*t).field = (*s).field;
    free(s as *mut libc::c_void);
    free(t as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_self_store_self() {
    let s = calloc(
        1i32 as libc::c_ulong,
        ::std::mem::size_of::<S>() as libc::c_ulong,
    ) as *mut S;
    (*s).field4.field4 = (*s).field4.field4;
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_self_store_self_inter() {
    let s = calloc(
        1i32 as libc::c_ulong,
        ::std::mem::size_of::<S>() as libc::c_ulong,
    ) as *mut S;
    let y = (*s).field;
    (*s).field = y;
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_ptr_int_ptr() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let x = s as usize;
    s = x as *mut libc::c_void;
    free(s);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_value() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let ps = std::ptr::addr_of!(s);
    free(*ps);
}
#[no_mangle]
pub unsafe extern "C" fn test_store_value() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let t = s;
    let mut ps = std::ptr::addr_of_mut!(s);
    *ps = t;
    free(s);
}
#[no_mangle]
pub unsafe extern "C" fn test_store_value_field() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    let t = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*t).field3 = s;
    (*s).field3 = (*t).field3;
    free(t as *mut libc::c_void);
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_value_store_value() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let ps = std::ptr::addr_of_mut!(s);
    *ps = *ps;
    free(*ps);
}
#[no_mangle]
pub unsafe extern "C" fn insertion_sort(n: libc::c_int, p: *mut libc::c_int) {
    let mut i: libc::c_int = 1 as libc::c_int;
    while i < n {
        let tmp: libc::c_int = *p.offset(i as isize);
        let mut j: libc::c_int = i;
        while j > 0 as libc::c_int && *p.offset((j - 1 as libc::c_int) as isize) > tmp {
            *p.offset(j as isize) = *p.offset((j - 1 as libc::c_int) as isize);
            j -= 1
        }
        *p.offset(j as isize) = tmp;
        i += 1
    }
}

/*
    This is a minimal breaking example from lighttpd where instrumented code failed
    to compile.
*/
#[no_mangle]
#[cold]
unsafe extern "C" fn log_error_va_list_impl(
    mut errh: *const libc::c_void,
    filename: *const libc::c_char,
    line: libc::c_uint,
    fmt: *const libc::c_char,
    mut ap: ::std::ffi::VaList,
    perr: libc::c_int,
) {
    let mut prefix: [libc::c_char; 40] = [0; 40];
    vsprintf(prefix.as_mut_ptr(), fmt, ap.as_va_list());
}
pub unsafe extern "C" fn log_error(
    errh: *mut libc::c_void,
    filename: *const libc::c_char,
    line: libc::c_uint,
    mut fmt: *const libc::c_char,
    mut args: ...
) {
    let mut ap: ::std::ffi::VaListImpl;
    ap = args.clone();
    log_error_va_list_impl(errh, filename, line, fmt, ap.as_va_list(), 0 as libc::c_int);
}

/*
    This is a minimal breaking example from lighttpd where instrumented code failed
    to compile.
*/
fn vsprintf(_: *mut libc::c_char, _: *const libc::c_char, _: ::std::ffi::VaList) -> libc::c_int {
    0
}
#[no_mangle]
pub unsafe extern "C" fn ErrorMsg(
    mut filename: *const libc::c_char,
    mut lineno: libc::c_int,
    mut format: *const libc::c_char,
    mut args: ...
) {
    let mut errmsg: [libc::c_char; 10000] = [0; 10000];
    let mut prefix: [libc::c_char; 40] = [0; 40];
    let mut ap: ::std::ffi::VaListImpl;
    ap = args.clone();
    if lineno > 0 as libc::c_int {
        sprintf(
            prefix.as_mut_ptr(),
            b"%.*s:%d: \0" as *const u8 as *const libc::c_char,
            30 as libc::c_int - 10 as libc::c_int,
            filename,
            lineno,
        );
    } else {
        sprintf(
            prefix.as_mut_ptr(),
            b"%.*s: \0" as *const u8 as *const libc::c_char,
            30 as libc::c_int - 10 as libc::c_int,
            filename,
        );
    }
    vsprintf(errmsg.as_mut_ptr(), format, ap.as_va_list());
}

unsafe fn main_0(mut argc: libc::c_int, mut argv: *mut *mut libc::c_char) -> libc::c_int {
    simple();
    exercise_allocator();
    simple_analysis();
    analysis2();
    inter_function_analysis();
    no_owner(0i32);
    free(global as *mut libc::c_void);
    no_owner(1i32);
    invalid();
    testing();
    simple1();

    lighttpd_test();

    test_malloc_free();
    test_malloc_free_cast();
    test_arg();
    test_arg_rec();
    test_shared_ref();
    test_unique_ref();
    test_realloc_reassign();
    test_realloc_fresh();
    test_load_addr();
    test_load_addr();
    test_overwrite();
    test_store_addr();
    test_load_other_store_self();
    test_load_self_store_self();
    test_load_self_store_self_inter();
    test_ptr_int_ptr();
    test_load_value();
    test_store_value();
    test_store_value_field();
    test_load_value_store_value();
    let nums = &mut [2i32, 5i32, 3i32, 1i32, 6i32];
    insertion_sort(nums.len() as libc::c_int, nums as *mut libc::c_int);
    test_ref_field();
    test_addr_taken();
    let mut t = T {
        field: 0i32,
        field2: 0u64,
        field3: 0 as *const S,
        field4: 0i32,
    };
    test_addr_taken_arg(t);
    test_addr_taken_loop();
    test_addr_taken_cond(true);
    test_addr_taken_cond(false);
    test_addr_taken_init_cond(true);
    test_addr_taken_init_cond(false);
    return 0i32;
}

/*
    This is a minimal breaking example where instrumented code failed
    to compile.
*/
pub fn instrument_args_loop() {
    let mut args: Vec<*mut libc::c_char> = Vec::new();
    for arg in ::std::env::args() {}
}

pub fn main() {
    let args = ::std::env::args()
        .map(|arg| ::std::ffi::CString::new(arg).expect("Failed to convert argument into CString."))
        .collect::<Vec<_>>();
    let mut args = args
        .iter()
        .map(|arg| arg.as_ptr() as *mut libc::c_char)
        .chain(::std::iter::once(::std::ptr::null_mut()))
        .collect::<Vec<_>>();
    unsafe {
        main_0(
            (args.len() - 1) as libc::c_int,
            args.as_mut_ptr() as *mut *mut libc::c_char,
        );
    }
}
