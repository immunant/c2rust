#![feature(rustc_private)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_mut)]
#![allow(unused_imports)]
#![allow(unused_variables)]

extern crate libc;

use libc::*;
use std::mem;
pub type size_t = libc::c_ulong;

extern "C" {
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(__ptr: *mut libc::c_void);
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
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
    pub fd: libc::c_int,
    pub events: libc::c_int,
    pub fde_ndx: libc::c_int,
}

pub type fdnode = fdnode_st;

unsafe extern "C" fn fdnode_init() -> *mut libc::c_void /*TODO: handle *mut fdnode */ {
    let fdn /*TODO: handle : *mut fdnode */ = calloc(
        1 as libc::c_int as libc::c_ulong,
        ::std::mem::size_of::<fdnode>() as libc::c_ulong,
    ) /* TODO: handle cast as *mut fdnode */;
    if fdn.is_null() {
        // println!("It's null");
    }
    return fdn;
}

#[no_mangle]
pub unsafe extern "C" fn connection_accepted(
    mut srv: *mut server,
    mut cnt: libc::c_int,
    fdn: *mut fdnode,     // TODO: remove when casts from c_void are handled
    con: *mut connection, // TODO: remove when casts from c_void are handled
) -> *mut connection {
    // let con = malloc(::std::mem::size_of::<connection>() as libc::c_ulong); // TODO: handle as *mut connection;
    (*con).fd = cnt;
    (*con).fdn = fdn;
    let x = &*(*(*(*srv).ev).fdarray).offset(0);
    (*(*(*(*srv).ev).fdarray).offset(0)).fd = 0;
    return con;
}

unsafe extern "C" fn connection_close(
    mut srv: *mut server,
    mut con: *mut libc::c_void, /* TODO: handle *mut connection */
) {
    free(con /* TODO: handle cast as *mut libc::c_void */);
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
    // *fresh1 = 0 as *mut fdnode;
    // fdnode_free(fdn);
}

unsafe extern "C" fn fdnode_free(fdn: *mut libc::c_void) {
    free(fdn /* TODO: handle cast as *mut libc::c_void */);
}

pub unsafe extern "C" fn lighttpd_test() {
    let fdarr = malloc(::std::mem::size_of::<*mut fdnode>() as libc::c_ulong); // TODO: handle cast as *mut *mut fdnode;
    let fdes = malloc(::std::mem::size_of::<fdevents>() as libc::c_ulong); // TODO: handle cast as *mut fdevents;
    free(fdarr /* TODO: handle cast as *mut libc::c_void */);
    free(fdes /* TODO: handle cast as *mut libc::c_void */);
}

fn main() {
    unsafe {
        lighttpd_test();
    }
}
