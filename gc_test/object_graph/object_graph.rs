#![allow(dead_code, mutable_transmutes, non_camel_case_types, non_snake_case, non_upper_case_globals, unused_assignments, unused_mut)]

use std::sync::atomic::{AtomicI32, Ordering};

extern "C" {
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn printf(_: *const libc::c_char, _: ...) -> libc::c_int;
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct session {
    pub id: libc::c_int,
    pub links: links,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct links {
    pub tqh_first: *mut link,
    pub tqh_last: *mut *mut link,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct link {
    pub session: *mut session,
    pub window: *mut window,
    pub session_entry: tailq_entry_link,
    pub window_entry: tailq_entry_link,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tailq_entry_link {
    pub tqe_next: *mut link,
    pub tqe_prev: *mut *mut link,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct window {
    pub id: libc::c_int,
    pub links: links,
}
static next_session_id: AtomicI32 = AtomicI32::new(0 as libc::c_int);
static next_window_id: AtomicI32 = AtomicI32::new(0 as libc::c_int);
#[no_mangle]
pub unsafe extern "C" fn session_new() -> *mut session {
    let mut sess: *mut session = calloc(
        1 as libc::c_int as libc::c_ulong,
        ::core::mem::size_of::<session>() as libc::c_ulong,
    ) as *mut session;
    next_session_id.fetch_add(1, Ordering::Relaxed);
    (*sess).id = next_session_id.load(Ordering::Relaxed);
    printf(b"new session %d\n\0" as *const u8 as *const libc::c_char, (*sess).id);
    (*sess).links.tqh_first = 0 as *mut link;
    (*sess).links.tqh_last = &mut (*sess).links.tqh_first;
    window_new(sess);
    return sess;
}
#[no_mangle]
pub unsafe extern "C" fn session_delete(mut sess: *mut session) {
    printf(b"delete session %d\n\0" as *const u8 as *const libc::c_char, (*sess).id);
    let mut l: *mut link = (*sess).links.tqh_first;
    while !l.is_null() {
        let mut next: *mut link = (*l).session_entry.tqe_next;
        link_delete(l, 2 as libc::c_int);
        l = next;
    }
    printf(b"deleted session %d\n\0" as *const u8 as *const libc::c_char, (*sess).id);
    free(sess as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn session_render(mut sess: *mut session) {
    printf(b"session %d windows: \0" as *const u8 as *const libc::c_char, (*sess).id);
    let mut l: *mut link = 0 as *mut link;
    let mut first: libc::c_int = 1 as libc::c_int;
    l = (*sess).links.tqh_first;
    while !l.is_null() {
        if first != 0 {
            first = 0 as libc::c_int;
            printf(b"%d\0" as *const u8 as *const libc::c_char, (*(*l).window).id);
        } else {
            printf(b", %d\0" as *const u8 as *const libc::c_char, (*(*l).window).id);
        }
        l = (*l).session_entry.tqe_next;
    }
    if first != 0 {
        printf(b"(none)\n\0" as *const u8 as *const libc::c_char);
    } else {
        printf(b"\n\0" as *const u8 as *const libc::c_char);
    };
}
#[no_mangle]
pub unsafe extern "C" fn window_new(mut sess: *mut session) -> *mut window {
    let mut win: *mut window = calloc(
        1 as libc::c_int as libc::c_ulong,
        ::core::mem::size_of::<window>() as libc::c_ulong,
    ) as *mut window;
    next_window_id.fetch_add(1, Ordering::Relaxed);
    (*win).id = next_window_id.load(Ordering::Relaxed);
    printf(b"new window %d\n\0" as *const u8 as *const libc::c_char, (*win).id);
    (*win).links.tqh_first = 0 as *mut link;
    (*win).links.tqh_last = &mut (*win).links.tqh_first;
    window_link(win, sess);
    return win;
}
#[no_mangle]
pub unsafe extern "C" fn window_delete(mut win: *mut window) {
    printf(b"delete window %d\n\0" as *const u8 as *const libc::c_char, (*win).id);
    let mut l: *mut link = (*win).links.tqh_first;
    while !l.is_null() {
        let mut next: *mut link = (*l).window_entry.tqe_next;
        link_delete(l, 1 as libc::c_int);
        l = next;
    }
    printf(b"deleted window %d\n\0" as *const u8 as *const libc::c_char, (*win).id);
    free(win as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn window_link(mut win: *mut window, mut sess: *mut session) {
    printf(
        b"link session %d, window %d\n\0" as *const u8 as *const libc::c_char,
        (*sess).id,
        (*win).id,
    );
    let mut link: *mut link = calloc(
        1 as libc::c_int as libc::c_ulong,
        ::core::mem::size_of::<link>() as libc::c_ulong,
    ) as *mut link;
    (*link).session = sess;
    (*link).window = win;
    (*link).session_entry.tqe_next = 0 as *mut link;
    (*link).session_entry.tqe_prev = (*sess).links.tqh_last;
    *(*sess).links.tqh_last = link;
    (*sess).links.tqh_last = &mut (*link).session_entry.tqe_next;
    (*link).window_entry.tqe_next = 0 as *mut link;
    (*link).window_entry.tqe_prev = (*win).links.tqh_last;
    *(*win).links.tqh_last = link;
    (*win).links.tqh_last = &mut (*link).window_entry.tqe_next;
}
#[no_mangle]
pub unsafe extern "C" fn window_unlink(mut win: *mut window, mut sess: *mut session) {
    let mut l: *mut link = 0 as *mut link;
    l = (*win).links.tqh_first;
    while !l.is_null() {
        if (*l).session == sess {
            break;
        }
        l = (*l).window_entry.tqe_next;
    }
    if !l.is_null() {
        link_delete(l, 1 as libc::c_int | 2 as libc::c_int);
    }
}
#[no_mangle]
pub unsafe extern "C" fn link_delete(mut l: *mut link, mut cleanup_flags: libc::c_int) {
    printf(
        b"unlink session %d, window %d\n\0" as *const u8 as *const libc::c_char,
        (*(*l).session).id,
        (*(*l).window).id,
    );
    if cleanup_flags & 1 as libc::c_int != 0 {
        if !((*l).session_entry.tqe_next).is_null() {
            (*(*l).session_entry.tqe_next)
                .session_entry
                .tqe_prev = (*l).session_entry.tqe_prev;
        } else {
            (*(*l).session).links.tqh_last = (*l).session_entry.tqe_prev;
        }
        *(*l).session_entry.tqe_prev = (*l).session_entry.tqe_next;
        if ((*(*l).session).links.tqh_first).is_null() {
            session_delete((*l).session);
        }
    }
    if cleanup_flags & 2 as libc::c_int != 0 {
        if !((*l).window_entry.tqe_next).is_null() {
            (*(*l).window_entry.tqe_next)
                .window_entry
                .tqe_prev = (*l).window_entry.tqe_prev;
        } else {
            (*(*l).window).links.tqh_last = (*l).window_entry.tqe_prev;
        }
        *(*l).window_entry.tqe_prev = (*l).window_entry.tqe_next;
        if ((*(*l).window).links.tqh_first).is_null() {
            window_delete((*l).window);
        }
    }
    free(l as *mut libc::c_void);
}
unsafe fn main_0() -> libc::c_int {
    let mut sess1: *mut session = session_new();
    let mut sess2: *mut session = session_new();
    printf(
        b"\ndelete a window explicitly, which removes it from its sessions\n\0"
            as *const u8 as *const libc::c_char,
    );
    let mut win3: *mut window = window_new(sess1);
    window_link(win3, sess2);
    session_render(sess1);
    session_render(sess2);
    window_delete(win3);
    session_render(sess1);
    session_render(sess2);
    printf(
        b"\ndelete a window implicitly by removing it from all sessions\n\0" as *const u8
            as *const libc::c_char,
    );
    let mut win4: *mut window = window_new(sess1);
    window_unlink(win4, sess1);
    session_render(sess1);
    session_render(sess2);
    printf(
        b"\ndelete a session implicitly by removing all of its windows\n\0" as *const u8
            as *const libc::c_char,
    );
    let mut sess3: *mut session = session_new();
    let mut win5: *mut window = (*(*sess3).links.tqh_first).window;
    window_unlink(win5, sess3);
    printf(
        b"\ndelete a session implicitly by deleting all of its windows\n\0" as *const u8
            as *const libc::c_char,
    );
    let mut sess4: *mut session = session_new();
    let mut win6: *mut window = (*(*sess4).links.tqh_first).window;
    window_delete(win6);
    printf(
        b"\ndelete sessions, which removes and deletes all of their windows\n\0"
            as *const u8 as *const libc::c_char,
    );
    let mut win7: *mut window = window_new(sess1);
    window_link(win7, sess2);
    session_render(sess1);
    session_render(sess2);
    session_delete(sess2);
    session_delete(sess1);
    return 0;
}
pub fn main() {
    unsafe { ::std::process::exit(main_0() as i32) }
}
