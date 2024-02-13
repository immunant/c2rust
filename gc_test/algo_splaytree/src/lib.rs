use ::libc;

use std::cell::Cell;

pub mod drc;
use self::drc::{Drc, NullableDrc};

pub mod cell2;
use self::cell2::Cell2;

extern "C" {
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn __assert_fail(
        __assertion: *const libc::c_char,
        __file: *const libc::c_char,
        __line: libc::c_uint,
        __function: *const libc::c_char,
    ) -> !;
}
#[derive(Clone)]
#[repr(C)]
pub struct tree_node {
    pub left: Cell2<NullableDrc<tree_node>>,
    pub right: Cell2<NullableDrc<tree_node>>,
    pub key: Cell<libc::c_int>,
    //pub data: Cell2<NullableDrc<libc::c_void>>,
    pub data: Cell<usize>,
}
pub type splay_tree = tree_node;
#[no_mangle]
pub extern "C" fn splaytree_splay(
    mut t: NullableDrc<splay_tree>,
    mut i: libc::c_int,
) -> NullableDrc<splay_tree> {
    let mut N: Drc<splay_tree> = Drc::new(splay_tree {
        left: Cell2::new(NullableDrc::null()),
        right: Cell2::new(NullableDrc::null()),
        key: Cell::new(0),
        data: Cell::new(0),
    });
    let mut l: NullableDrc<splay_tree> = NullableDrc::null();
    let mut r: NullableDrc<splay_tree> = NullableDrc::null();
    let mut y: NullableDrc<splay_tree> = NullableDrc::null();
    let mut comp: libc::c_int = 0;
    if t.is_null() {
        return t;
    }
    N.get().right.set(NullableDrc::null());
    N.get().left.set(N.get().right.get());
    r = N.clone().into();
    l = r.clone();
    loop {
        comp = i - (*t.get()).key.get();
        if comp < 0 as libc::c_int {
            if ((*t.get()).left.get()).is_null() {
                break;
            }
            if i - (*(*t.get()).left.get().get()).key.get() < 0 as libc::c_int {
                y = (*t.get()).left.get();
                (*t.get()).left.set((*y.get()).right.get());
                (*y.get()).right.set(t.clone());
                t = y.clone();
                if ((*t.get()).left.get()).is_null() {
                    break;
                }
            }
            (*r.get()).left.set(t.clone());
            r = t.clone();
            let tmp = (*t.get()).left.get();
            t = tmp;
        } else {
            if !(comp > 0 as libc::c_int) {
                break;
            }
            if ((*t.get()).right.get()).is_null() {
                break;
            }
            if i - (*(*t.get()).right.get().get()).key.get() > 0 as libc::c_int {
                y = (*t.get()).right.get();
                (*t.get()).right.set((*y.get()).left.get());
                (*y.get()).left.set(t.clone());
                t = y.clone();
                if ((*t.get()).right.get()).is_null() {
                    break;
                }
            }
            (*l.get()).right.set(t.clone());
            l = t.clone();
            let tmp = (*t.get()).right.get();
            t = tmp;
        }
    }
    (*l.get()).right.set((*t.get()).left.get());
    (*r.get()).left.set((*t.get()).right.get());
    (*t.get()).left.set(N.get().right.get());
    (*t.get()).right.set(N.get().left.get());
    return t;
}
#[no_mangle]
pub extern "C" fn splaytree_insert(
    mut t: NullableDrc<splay_tree>,
    mut i: libc::c_int,
    mut data: usize,
) -> NullableDrc<splay_tree> {
    let mut new: NullableDrc<splay_tree> = NullableDrc::null();
    if !t.is_null() {
        t = splaytree_splay(t, i);
        if i - (*t.get()).key.get() == 0 as libc::c_int {
            return t;
        }
    }
    //new = malloc(::std::mem::size_of::<splay_tree>() as libc::c_ulong)
    //    as *mut splay_tree;
    new = NullableDrc::new(splay_tree {
        left: Cell2::new(NullableDrc::null()),
        right: Cell2::new(NullableDrc::null()),
        key: Cell::new(0),
        data: Cell::new(0),
    });
    assert!(!new.is_null());
    /*
    if !new.is_null() {} else {
        __assert_fail(
            b"new\0" as *const u8 as *const libc::c_char,
            b"src/algo_splaytree.c\0" as *const u8 as *const libc::c_char,
            121 as libc::c_int as libc::c_uint,
            (*::std::mem::transmute::<
                &[u8; 56],
                &[libc::c_char; 56],
            >(b"splay_tree *splaytree_insert(splay_tree *, int, void *)\0"))
                .as_ptr(),
        );
    }
    */
    if t.is_null() {
        (*new.get()).right.set(NullableDrc::null());
        (*new.get()).left.set((*new.get()).right.get());
    } else if i - (*t.get()).key.get() < 0 as libc::c_int {
        (*new.get()).left.set((*t.get()).left.get());
        (*new.get()).right.set(t.clone());
        (*t.get()).left.set(NullableDrc::null());
    } else {
        (*new.get()).right.set((*t.get()).right.get());
        (*new.get()).left.set(t.clone());
        (*t.get()).right.set(NullableDrc::null());
    }
    (*new.get()).key.set(i);
    (*new.get()).data.set(data);
    return new;
}
/*
#[no_mangle]
pub unsafe extern "C" fn splaytree_delete(
    mut t: *mut splay_tree,
    mut i: libc::c_int,
) -> *mut splay_tree {
    let mut x: *mut splay_tree = 0 as *mut splay_tree;
    if t.is_null() {
        return 0 as *mut splay_tree;
    }
    t = splaytree_splay(t, i);
    if i - (*t).key == 0 as libc::c_int {
        if ((*t).left).is_null() {
            x = (*t).right;
        } else {
            x = splaytree_splay((*t).left, i);
            (*x).right = (*t).right;
        }
        free(t as *mut libc::c_void);
        return x;
    } else {
        return t
    };
}
*/
