#![allow(dead_code, mutable_transmutes, non_camel_case_types, non_snake_case, non_upper_case_globals, unused_assignments, unused_mut)]

use std::cell::Cell;
use std::sync::atomic::{AtomicI32, Ordering};
use gc_lib::cell2::Cell2;
use gc_lib::drc_gc::{NullableDrc, NullableSubDrc, BreakCycles};

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
pub fn session_new() -> NullableDrc<session> {
    let sess: NullableDrc<session> = NullableDrc::new(session {
        id: Cell::new(0),
        links: links {
            tqh_first: Cell2::new(NullableDrc::null()),
            tqh_last: Cell2::new(NullableSubDrc::null()),
        },
    });
    next_session_id.fetch_add(1, Ordering::Relaxed);
    sess.id.set(next_session_id.load(Ordering::Relaxed));
    println!("new session {}", sess.id.get());
    sess.links.tqh_first.set(NullableDrc::null());
    sess.links.tqh_last.set(sess.project(|sess| &sess.links.tqh_first));
    window_new(sess);
    sess
}
#[no_mangle]
pub fn session_delete(mut sess: NullableDrc<session>) {
    println!("delete session {}", sess.id.get());
    let mut l: NullableDrc<link> = sess.links.tqh_first.get();
    while !l.is_null() {
        let mut next: NullableDrc<link> = l.session_entry.tqe_next.get();
        link_delete(l, 2 as libc::c_int);
        l = next;
    }
    println!("deleted session {}", sess.id.get());
    sess.drop_data();
}
#[no_mangle]
pub fn session_render(mut sess: NullableDrc<session>) {
    print!("session {} windows: ", sess.id.get());
    let mut l: NullableDrc<link> = NullableDrc::null();
    let mut first: libc::c_int = 1 as libc::c_int;
    l = sess.links.tqh_first.get();
    while !l.is_null() {
        if first != 0 {
            first = 0 as libc::c_int;
            print!("{}", l.window.get().id.get());
        } else {
            print!(", {}", l.window.get().id.get());
        }
        l = l.session_entry.tqe_next.get();
    }
    if first != 0 {
        println!("(none)");
    } else {
        println!();
    };
}
#[no_mangle]
pub fn window_new(mut sess: NullableDrc<session>) -> NullableDrc<window> {
    let mut win: NullableDrc<window> = NullableDrc::new(window {
        id: Cell::new(0),
        links: links {
            tqh_first: Cell2::new(NullableDrc::null()),
            tqh_last: Cell2::new(NullableSubDrc::null()),
        },
    });
    next_window_id.fetch_add(1, Ordering::Relaxed);
    win.id.set(next_window_id.load(Ordering::Relaxed));
    println!("new window {}", win.id.get());
    win.links.tqh_first.set(NullableDrc::null());
    win.links.tqh_last.set(win.project(|win| &win.links.tqh_first));
    window_link(win, sess);
    win
}
#[no_mangle]
pub fn window_delete(mut win: NullableDrc<window>) {
    println!("delete window {}", win.id.get());
    let mut l: NullableDrc<link> = win.links.tqh_first.get();
    while !l.is_null() {
        let mut next: NullableDrc<link> = l.window_entry.tqe_next.get();
        link_delete(l, 1 as libc::c_int);
        l = next;
    }
    println!("deleted window {}", win.id.get());
    win.drop_data();
}
#[no_mangle]
pub fn window_link(mut win: NullableDrc<window>, mut sess: NullableDrc<session>) {
    println!(
        "link session {}, window {}",
        sess.id.get(),
        win.id.get(),
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
    link.session.set(sess);
    link.window.set(win);
    link.session_entry.tqe_next.set(NullableDrc::null());
    link.session_entry.tqe_prev.set(sess.links.tqh_last.get());
    (*sess.links.tqh_last.get()).set(link);
    sess.links.tqh_last.set(link.project(|link| &link.session_entry.tqe_next));
    link.window_entry.tqe_next.set(NullableDrc::null());
    link.window_entry.tqe_prev.set(win.links.tqh_last.get());
    (*win.links.tqh_last.get()).set(link);
    win.links.tqh_last.set(link.project(|link| &link.window_entry.tqe_next));
}
#[no_mangle]
pub fn window_unlink(mut win: NullableDrc<window>, mut sess: NullableDrc<session>) {
    let mut l: NullableDrc<link> = NullableDrc::null();
    l = win.links.tqh_first.get();
    while !l.is_null() {
        if NullableDrc::ptr_eq(l.session.get(), sess) {
            break;
        }
        l = l.window_entry.tqe_next.get();
    }
    if !l.is_null() {
        link_delete(l, 1 as libc::c_int | 2 as libc::c_int);
    }
}
#[no_mangle]
pub fn link_delete(mut l: NullableDrc<link>, mut cleanup_flags: libc::c_int) {
    println!(
        "unlink session {}, window {}",
        l.session.get().id.get(),
        l.window.get().id.get(),
    );
    if cleanup_flags & 1 as libc::c_int != 0 {
        if !(l.session_entry.tqe_next.get()).is_null() {
            l.session_entry.tqe_next.get()
                .session_entry
                .tqe_prev.set(l.session_entry.tqe_prev.get());
        } else {
            l.session.get().links.tqh_last.set(l.session_entry.tqe_prev.get());
        }
        (*l.session_entry.tqe_prev.get()).set(l.session_entry.tqe_next.get());
        if (l.session.get().links.tqh_first.get()).is_null() {
            session_delete(l.session.get());
        }
    }
    if cleanup_flags & 2 as libc::c_int != 0 {
        if !(l.window_entry.tqe_next.get()).is_null() {
            l.window_entry.tqe_next.get()
                .window_entry
                .tqe_prev.set(l.window_entry.tqe_prev.get());
        } else {
            l.window.get().links.tqh_last.set(l.window_entry.tqe_prev.get());
        }
        (*l.window_entry.tqe_prev.get()).set(l.window_entry.tqe_next.get());
        if (l.window.get().links.tqh_first.get()).is_null() {
            window_delete(l.window.get());
        }
    }
    l.break_cycles();
}
fn main_0() -> libc::c_int {
    let mut sess1: NullableDrc<session> = session_new();
    let mut sess2: NullableDrc<session> = session_new();
    println!("\ndelete a window explicitly, which removes it from its sessions");
    let mut win3: NullableDrc<window> = window_new(sess1);
    window_link(win3, sess2);
    session_render(sess1);
    session_render(sess2);
    window_delete(win3);
    session_render(sess1);
    session_render(sess2);
    println!("\ndelete a window implicitly by removing it from all sessions");
    let mut win4: NullableDrc<window> = window_new(sess1);
    window_unlink(win4, sess1);
    session_render(sess1);
    session_render(sess2);
    println!("\ndelete a session implicitly by removing all of its windows");
    let mut sess3: NullableDrc<session> = session_new();
    let mut win5: NullableDrc<window> = sess3.links.tqh_first.get().window.get();
    window_unlink(win5, sess3);
    println!("\ndelete a session implicitly by deleting all of its windows");
    let mut sess4: NullableDrc<session> = session_new();
    let mut win6: NullableDrc<window> = sess4.links.tqh_first.get().window.get();
    window_delete(win6);
    println!("\ndelete sessions, which removes and deletes all of their windows");
    let mut win7: NullableDrc<window> = window_new(sess1);
    window_link(win7, sess2);
    session_render(sess1);
    session_render(sess2);
    session_delete(sess2);
    session_delete(sess1);
    0
}
pub fn main() {
    ::std::process::exit(main_0() as i32)
}
