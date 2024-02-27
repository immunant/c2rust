#![allow(dead_code, mutable_transmutes, non_camel_case_types, non_snake_case, non_upper_case_globals, unused_assignments, unused_mut)]

use std::cell::Cell;
use std::sync::atomic::{AtomicI32, Ordering};
use gc_lib::cell2::Cell2;
use gc_lib::drc::{Drc, NullableDrc, NullableSubDrc, BreakCycles};

extern "C" {
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn printf(_: *const libc::c_char, _: ...) -> libc::c_int;
}
#[derive(Clone)]
#[repr(C)]
pub struct session {
    pub id: Cell<libc::c_int>,
    pub links: links,
}
#[derive(Clone)]
#[repr(C)]
pub struct links {
    pub tqh_first: Cell2<NullableDrc<link>>,
    pub tqh_last: Cell2<NullableSubDrc<Cell2<NullableDrc<link>>>>,
}
#[derive(Clone)]
#[repr(C)]
pub struct link {
    pub session: Cell2<NullableDrc<session>>,
    pub window: Cell2<NullableDrc<window>>,
    pub session_entry: tailq_entry_link,
    pub window_entry: tailq_entry_link,
}
#[derive(Clone)]
#[repr(C)]
pub struct tailq_entry_link {
    pub tqe_next: Cell2<NullableDrc<link>>,
    pub tqe_prev: Cell2<NullableSubDrc<Cell2<NullableDrc<link>>>>,
}
#[derive(Clone)]
#[repr(C)]
pub struct window {
    pub id: Cell<libc::c_int>,
    pub links: links,
}
static next_session_id: AtomicI32 = AtomicI32::new(0 as libc::c_int);
static next_window_id: AtomicI32 = AtomicI32::new(0 as libc::c_int);

impl BreakCycles for session {
    fn break_cycles(&self) {
        self.links.break_cycles();
    }
}

impl BreakCycles for links {
    fn break_cycles(&self) {
        self.tqh_first.set(NullableDrc::null());
        self.tqh_last.set(NullableSubDrc::null());
    }
}

impl BreakCycles for link {
    fn break_cycles(&self) {
        self.session.set(NullableDrc::null());
        self.window.set(NullableDrc::null());
        self.session_entry.break_cycles();
        self.window_entry.break_cycles();
    }
}

impl BreakCycles for tailq_entry_link {
    fn break_cycles(&self) {
        self.tqe_next.set(NullableDrc::null());
        self.tqe_prev.set(NullableSubDrc::null());
    }
}

impl BreakCycles for window {
    fn break_cycles(&self) {
        self.links.break_cycles();
    }
}

#[no_mangle]
pub unsafe extern "C" fn session_new() -> Drc<session> {
    let sess: Drc<session> = Drc::new(session {
        id: Cell::new(0),
        links: links {
            tqh_first: Cell2::new(NullableDrc::null()),
            tqh_last: Cell2::new(NullableSubDrc::null()),
        },
    });
    next_session_id.fetch_add(1, Ordering::Relaxed);
    (*sess).id.set(next_session_id.load(Ordering::Relaxed));
    printf(b"new session %d\n\0" as *const u8 as *const libc::c_char, (*sess).id.get());
    (*sess).links.tqh_first.set(NullableDrc::null());
    (*sess).links.tqh_last.set(sess.clone().project(|sess| &(*sess).links.tqh_first).into());
    window_new(sess.clone());
    return sess;
}
#[no_mangle]
pub unsafe extern "C" fn session_delete(mut sess: Drc<session>) {
    printf(b"delete session %d\n\0" as *const u8 as *const libc::c_char, (*sess).id.get());
    let mut l: NullableDrc<link> = (*sess).links.tqh_first.get();
    while !l.is_null() {
        let mut next: NullableDrc<link> = (*l).session_entry.tqe_next.get();
        link_delete(l, 2 as libc::c_int);
        l = next;
    }
    printf(b"deleted session %d\n\0" as *const u8 as *const libc::c_char, (*sess).id.get());
    sess.drop_data();
}
#[no_mangle]
pub unsafe extern "C" fn session_render(mut sess: Drc<session>) {
    printf(b"session %d windows: \0" as *const u8 as *const libc::c_char, (*sess).id.get());
    let mut l: NullableDrc<link> = NullableDrc::null();
    let mut first: libc::c_int = 1 as libc::c_int;
    l = (*sess).links.tqh_first.get();
    while !l.is_null() {
        if first != 0 {
            first = 0 as libc::c_int;
            printf(b"%d\0" as *const u8 as *const libc::c_char, (*(*l).window.get()).id.get());
        } else {
            printf(b", %d\0" as *const u8 as *const libc::c_char, (*(*l).window.get()).id.get());
        }
        l = (*l).session_entry.tqe_next.get();
    }
    if first != 0 {
        printf(b"(none)\n\0" as *const u8 as *const libc::c_char);
    } else {
        printf(b"\n\0" as *const u8 as *const libc::c_char);
    };
}
#[no_mangle]
pub unsafe extern "C" fn window_new(mut sess: Drc<session>) -> Drc<window> {
    let mut win: Drc<window> = Drc::new(window {
        id: Cell::new(0),
        links: links {
            tqh_first: Cell2::new(NullableDrc::null()),
            tqh_last: Cell2::new(NullableSubDrc::null()),
        },
    });
    next_window_id.fetch_add(1, Ordering::Relaxed);
    (*win).id.set(next_window_id.load(Ordering::Relaxed));
    printf(b"new window %d\n\0" as *const u8 as *const libc::c_char, (*win).id.get());
    (*win).links.tqh_first.set(NullableDrc::null());
    (*win).links.tqh_last.set(win.clone().project(|win| &(*win).links.tqh_first).into());
    window_link(win.clone(), sess);
    return win;
}
#[no_mangle]
pub unsafe extern "C" fn window_delete(mut win: Drc<window>) {
    printf(b"delete window %d\n\0" as *const u8 as *const libc::c_char, (*win).id.get());
    let mut l: NullableDrc<link> = (*win).links.tqh_first.get();
    while !l.is_null() {
        let mut next: NullableDrc<link> = (*l).window_entry.tqe_next.get();
        link_delete(l, 1 as libc::c_int);
        l = next;
    }
    printf(b"deleted window %d\n\0" as *const u8 as *const libc::c_char, (*win).id.get());
    win.drop_data();
}
#[no_mangle]
pub unsafe extern "C" fn window_link(mut win: Drc<window>, mut sess: Drc<session>) {
    printf(
        b"link session %d, window %d\n\0" as *const u8 as *const libc::c_char,
        (*sess).id.get(),
        (*win).id.get(),
    );
    let mut link: NullableDrc<link> = NullableDrc::new(link {
        session: Cell2::new(NullableDrc::null()),
        window: Cell2::new(NullableDrc::null()),
        session_entry: tailq_entry_link {
            tqe_next: Cell2::new(NullableDrc::null()),
            tqe_prev: Cell2::new(NullableSubDrc::null()),
        },
        window_entry: tailq_entry_link {
            tqe_next: Cell2::new(NullableDrc::null()),
            tqe_prev: Cell2::new(NullableSubDrc::null()),
        },
    });
    (*link).session.set(sess.clone().into());
    (*link).window.set(win.clone().into());
    (*link).session_entry.tqe_next.set(NullableDrc::null());
    (*link).session_entry.tqe_prev.set((*sess).links.tqh_last.get());
    (*(*sess).links.tqh_last.get()).set(link.clone());
    (*sess).links.tqh_last.set(link.clone().project(|link| &(*link).session_entry.tqe_next));
    (*link).window_entry.tqe_next.set(NullableDrc::null());
    (*link).window_entry.tqe_prev.set((*win).links.tqh_last.get());
    (*(*win).links.tqh_last.get()).set(link.clone());
    (*win).links.tqh_last.set(link.clone().project(|link| &(*link).window_entry.tqe_next));
}
#[no_mangle]
pub unsafe extern "C" fn window_unlink(mut win: Drc<window>, mut sess: Drc<session>) {
    let mut l: NullableDrc<link> = NullableDrc::null();
    l = (*win).links.tqh_first.get();
    while !l.is_null() {
        if Drc::ptr_eq(&(*l).session.get().into(), &sess) {
            break;
        }
        l = (*l).window_entry.tqe_next.get();
    }
    if !l.is_null() {
        link_delete(l, 1 as libc::c_int | 2 as libc::c_int);
    }
}
#[no_mangle]
pub unsafe extern "C" fn link_delete(mut l: NullableDrc<link>, mut cleanup_flags: libc::c_int) {
    printf(
        b"unlink session %d, window %d\n\0" as *const u8 as *const libc::c_char,
        (*(*l).session.get()).id.get(),
        (*(*l).window.get()).id.get(),
    );
    if cleanup_flags & 1 as libc::c_int != 0 {
        if !((*l).session_entry.tqe_next.get()).is_null() {
            (*(*l).session_entry.tqe_next.get())
                .session_entry
                .tqe_prev.set((*l).session_entry.tqe_prev.get());
        } else {
            (*(*l).session.get()).links.tqh_last.set((*l).session_entry.tqe_prev.get());
        }
        (*(*l).session_entry.tqe_prev.get()).set((*l).session_entry.tqe_next.get());
        if ((*(*l).session.get()).links.tqh_first.get()).is_null() {
            session_delete((*l).session.get().into());
        }
    }
    if cleanup_flags & 2 as libc::c_int != 0 {
        if !((*l).window_entry.tqe_next.get()).is_null() {
            (*(*l).window_entry.tqe_next.get())
                .window_entry
                .tqe_prev.set((*l).window_entry.tqe_prev.get());
        } else {
            (*(*l).window.get()).links.tqh_last.set((*l).window_entry.tqe_prev.get());
        }
        (*(*l).window_entry.tqe_prev.get()).set((*l).window_entry.tqe_next.get());
        if ((*(*l).window.get()).links.tqh_first.get()).is_null() {
            window_delete((*l).window.get().into());
        }
    }
    l.break_cycles();
}
unsafe fn main_0() -> libc::c_int {
    let mut sess1: Drc<session> = session_new();
    let mut sess2: Drc<session> = session_new();
    printf(
        b"\ndelete a window explicitly, which removes it from its sessions\n\0"
            as *const u8 as *const libc::c_char,
    );
    let mut win3: Drc<window> = window_new(sess1.clone());
    window_link(win3.clone(), sess2.clone());
    session_render(sess1.clone());
    session_render(sess2.clone());
    window_delete(win3);
    session_render(sess1.clone());
    session_render(sess2.clone());
    printf(
        b"\ndelete a window implicitly by removing it from all sessions\n\0" as *const u8
            as *const libc::c_char,
    );
    let mut win4: Drc<window> = window_new(sess1.clone());
    window_unlink(win4, sess1.clone());
    session_render(sess1.clone());
    session_render(sess2.clone());
    printf(
        b"\ndelete a session implicitly by removing all of its windows\n\0" as *const u8
            as *const libc::c_char,
    );
    let mut sess3: Drc<session> = session_new();
    let mut win5: Drc<window> = (*(*sess3).links.tqh_first.get()).window.get().into();
    window_unlink(win5, sess3);
    printf(
        b"\ndelete a session implicitly by deleting all of its windows\n\0" as *const u8
            as *const libc::c_char,
    );
    let mut sess4: Drc<session> = session_new();
    let mut win6: Drc<window> = (*(*sess4).links.tqh_first.get()).window.get().into();
    window_delete(win6);
    printf(
        b"\ndelete sessions, which removes and deletes all of their windows\n\0"
            as *const u8 as *const libc::c_char,
    );
    let mut win7: Drc<window> = window_new(sess1.clone());
    window_link(win7, sess2.clone());
    session_render(sess1.clone());
    session_render(sess2.clone());
    session_delete(sess2);
    session_delete(sess1);
    return 0;
}
pub fn main() {
    unsafe { ::std::process::exit(main_0() as i32) }
}
