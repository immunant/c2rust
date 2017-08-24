#![feature(custom_attribute, attr_literals)]

use std::mem;
use std::os::raw::c_void;
use std::ptr;

struct Forest {
    #[ownership_static(MOVE)]
    tree: *mut Tree,
    #[ownership_static(MOVE)]
    next: *mut Forest,
}

struct Tree {
    #[ownership_static()]
    data: i32,
    #[ownership_static(MOVE)]
    children: *mut Forest,
}

#[ownership_constraints(le(min(WRITE, _1), _0))]
#[ownership_mono("take", WRITE, MOVE)]
#[ownership_variant_of("old/8cd878b::get_children[0]")]
unsafe fn get_children_take(parent: *mut Tree) -> *mut Forest {
    let mut out = ptr::null_mut();
    get_children_err_take(parent, &mut out);
    out
}
#[ownership_mono("mut", WRITE, WRITE)]
#[ownership_variant_of("old/8cd878b::get_children[0]")]
unsafe fn get_children_mut(parent: *mut Tree) -> *mut Forest {
    let mut out = ptr::null_mut();
    get_children_err_mut(parent, &mut out);
    out
}
#[ownership_mono("", READ, READ)]
#[ownership_variant_of("old/8cd878b::get_children[0]")]
unsafe fn get_children(parent: *mut Tree) -> *mut Forest {
    let mut out = ptr::null_mut();
    get_children_err(parent, &mut out);
    out
}
#[ownership_constraints(le(WRITE, _1), le(min(WRITE, _2), _0))]
#[ownership_mono("take", WRITE, WRITE, MOVE)]
#[ownership_variant_of("old/8cd878b::get_children_err[0]")]
unsafe fn get_children_err_take(parent: *mut Tree, children_out: *mut *mut Forest) -> i32 {
    *children_out = (*parent).children;
    0
}
#[ownership_mono("mut", WRITE, WRITE, WRITE)]
#[ownership_variant_of("old/8cd878b::get_children_err[0]")]
unsafe fn get_children_err_mut(parent: *mut Tree, children_out: *mut *mut Forest) -> i32 {
    *children_out = (*parent).children;
    0
}

#[ownership_mono("", READ, WRITE, READ)]
#[ownership_variant_of("old/8cd878b::get_children_err[0]")]
unsafe fn get_children_err(parent: *mut Tree, children_out: *mut *mut Forest) -> i32 {
    *children_out = (*parent).children;
    0
}

fn main() {}
