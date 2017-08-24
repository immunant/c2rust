//! This file was annotated with `ownership_annotate`, but then half the annotations were deleted.
//! If all goes well, rerunning `ownership_annotate` should reintroduce the missing annotations,
//! without changing any.
//!
//! Note that each function either keeps all of its `mono` attributes or none of them.  The
//! analysis assumes the list of signatures in the `ownership_mono` attributes is exhaustive.

#![feature(custom_attribute, attr_literals)]

use std::mem;
use std::os::raw::c_void;
use std::ptr;

extern "C" {
    fn malloc(size: usize) -> *const c_void;
    fn free(ptr: *mut c_void);
}

struct Forest {
    #[ownership_static(MOVE)]
    tree: *mut Tree,
    #[ownership_static(MOVE)]
    next: *mut Forest,
}

struct Tree {
    data: i32,
    #[ownership_static(MOVE)]
    children: *mut Forest,
}

#[ownership_constraints()]
unsafe fn leaf(data: i32) -> *mut Tree {
    let t: *mut Tree = malloc(mem::size_of::<Tree>()) as *mut Tree;
    (*t).data = data;
    (*t).children = ptr::null_mut();
    t
}

#[ownership_mono("", WRITE, MOVE)]
unsafe fn add_child(parent: *mut Tree, child: *mut Tree) {
    let link: *mut Forest = malloc(mem::size_of::<Forest>()) as *mut Forest;
    (*link).tree = child;
    (*link).next = (*parent).children;
    (*parent).children = link;
}

unsafe fn delete_tree(tree: *mut Tree) {
    delete_forest((*tree).children);
    free(tree as *mut c_void);
}

#[ownership_constraints(le(MOVE, _0))]
#[ownership_mono("", MOVE)]
unsafe fn delete_forest(forest: *mut Forest) {
    delete_tree((*forest).tree);
    if !(*forest).next.is_null() {
        delete_forest((*forest).next);
    }
    free(forest as *mut c_void);
}

#[ownership_constraints(le(min(WRITE, _1), _0))]
unsafe fn get_children(parent: *mut Tree) -> *mut Forest {
    (*parent).children
}

#[ownership_mono("mut", WRITE, WRITE, WRITE)]
#[ownership_mono("", READ, WRITE, READ)]
unsafe fn get_children_err(parent: *mut Tree, children_out: *mut *mut Forest) -> i32 {
    *children_out = (*parent).children;
    0
}

unsafe fn head(f: *mut Forest) -> *mut Tree {
    (*f).tree
}

#[ownership_constraints(le(min(WRITE, _1), _0))]
#[ownership_mono("take", WRITE, MOVE)]
#[ownership_mono("mut", WRITE, WRITE)]
#[ownership_mono("", READ, READ)]
unsafe fn tail(f: *mut Forest) -> *mut Forest {
    (*f).next
}


unsafe fn delete_tree2(tree: *mut Tree) {
    let c: *mut Forest = get_children(tree);
    delete_forest2(c);
    free(tree as *mut c_void);
}

#[ownership_constraints(le(MOVE, _0))]
#[ownership_mono("", MOVE)]
unsafe fn delete_forest2(forest: *mut Forest) {
    let t: *mut Tree = head(forest);
    delete_tree2(t);
    let f: *mut Forest = tail(forest);
    if !f.is_null() {
        delete_forest2(f);
    }
    free(forest as *mut c_void);
}


#[ownership_constraints()]
fn main() {}
