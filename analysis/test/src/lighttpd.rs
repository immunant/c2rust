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
    let x = fdevents {
        fdarray: 0 as *mut *mut fdnode,
    };
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
