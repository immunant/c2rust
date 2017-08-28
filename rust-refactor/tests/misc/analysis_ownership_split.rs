#![feature(custom_attribute, attr_literals)]
use std::mem;
use std::os::raw::c_void;
use std::ptr;

extern "C" {
    #[ownership_mono("", MOVE)]
    #[ownership_variant_of("analysis_ownership/8cd878b::[0]::malloc[0]")]
    fn malloc(size: usize) -> *const c_void;
    #[ownership_mono("", MOVE)]
    #[ownership_variant_of("analysis_ownership/8cd878b::[0]::free[0]")]
    fn free(ptr: *mut c_void);
}

struct Forest {
    tree: *mut Tree,
    next: *mut Forest,
}

struct Tree {
    data: i32,
    children: *mut Forest,
}

#[ownership_mono("", MOVE)]
#[ownership_variant_of("analysis_ownership/8cd878b::leaf[0]")]
unsafe fn leaf(data: i32) -> *mut Tree {
    let t: *mut Tree = malloc(mem::size_of::<Tree>()) as *mut Tree;
    (*t).data = data;
    (*t).children = ptr::null_mut();
    t
}

#[ownership_mono("", WRITE, MOVE)]
#[ownership_variant_of("analysis_ownership/8cd878b::add_child[0]")]
unsafe fn add_child(parent: *mut Tree, child: *mut Tree) {
    let link: *mut Forest = malloc(mem::size_of::<Forest>()) as *mut Forest;
    (*link).tree = child;
    (*link).next = (*parent).children;
    (*parent).children = link;
}

#[ownership_mono("", MOVE)]
#[ownership_variant_of("analysis_ownership/8cd878b::delete_tree[0]")]
unsafe fn delete_tree(tree: *mut Tree) {
    delete_forest((*tree).children);
    free(tree as *mut c_void);
}

#[ownership_mono("", MOVE)]
#[ownership_variant_of("analysis_ownership/8cd878b::delete_forest[0]")]
unsafe fn delete_forest(forest: *mut Forest) {
    delete_tree((*forest).tree);
    if !(*forest).next.is_null() {
        delete_forest((*forest).next);
    }
    free(forest as *mut c_void);
}

#[ownership_mono("take", WRITE, MOVE)]
#[ownership_variant_of("analysis_ownership/8cd878b::get_children[0]")]
unsafe fn get_children_take(parent: *mut Tree) -> *mut Forest {
    (*parent).children
}
#[ownership_mono("mut", WRITE, WRITE)]
#[ownership_variant_of("analysis_ownership/8cd878b::get_children[0]")]
unsafe fn get_children_mut(parent: *mut Tree) -> *mut Forest {
    (*parent).children
}
#[ownership_mono("", READ, READ)]
#[ownership_variant_of("analysis_ownership/8cd878b::get_children[0]")]
unsafe fn get_children(parent: *mut Tree) -> *mut Forest {
    (*parent).children
}

#[ownership_mono("take", WRITE, WRITE, MOVE)]
#[ownership_variant_of("analysis_ownership/8cd878b::get_children_err[0]")]
unsafe fn get_children_err_take(parent: *mut Tree, children_out: *mut *mut Forest) -> i32 {
    *children_out = (*parent).children;
    0
}
#[ownership_mono("mut", WRITE, WRITE, WRITE)]
#[ownership_variant_of("analysis_ownership/8cd878b::get_children_err[0]")]
unsafe fn get_children_err_mut(parent: *mut Tree, children_out: *mut *mut Forest) -> i32 {
    *children_out = (*parent).children;
    0
}
#[ownership_mono("", READ, WRITE, READ)]
#[ownership_variant_of("analysis_ownership/8cd878b::get_children_err[0]")]
unsafe fn get_children_err(parent: *mut Tree, children_out: *mut *mut Forest) -> i32 {
    *children_out = (*parent).children;
    0
}

#[ownership_mono("take", WRITE, MOVE)]
#[ownership_variant_of("analysis_ownership/8cd878b::head[0]")]
unsafe fn head_take(f: *mut Forest) -> *mut Tree {
    (*f).tree
}
#[ownership_mono("mut", WRITE, WRITE)]
#[ownership_variant_of("analysis_ownership/8cd878b::head[0]")]
unsafe fn head_mut(f: *mut Forest) -> *mut Tree {
    (*f).tree
}
#[ownership_mono("", READ, READ)]
#[ownership_variant_of("analysis_ownership/8cd878b::head[0]")]
unsafe fn head(f: *mut Forest) -> *mut Tree {
    (*f).tree
}
#[ownership_mono("take", WRITE, MOVE)]
#[ownership_variant_of("analysis_ownership/8cd878b::tail[0]")]
unsafe fn tail_take(f: *mut Forest) -> *mut Forest {
    (*f).next
}
#[ownership_mono("mut", WRITE, WRITE)]
#[ownership_variant_of("analysis_ownership/8cd878b::tail[0]")]
unsafe fn tail_mut(f: *mut Forest) -> *mut Forest {
    (*f).next
}

#[ownership_mono("", READ, READ)]
#[ownership_variant_of("analysis_ownership/8cd878b::tail[0]")]
unsafe fn tail(f: *mut Forest) -> *mut Forest {
    (*f).next
}


#[ownership_mono("", MOVE)]
#[ownership_variant_of("analysis_ownership/8cd878b::delete_tree2[0]")]
unsafe fn delete_tree2(tree: *mut Tree) {
    let c: *mut Forest = get_children_take(tree);
    delete_forest2(c);
    free(tree as *mut c_void);
}

#[ownership_mono("", MOVE)]
#[ownership_variant_of("analysis_ownership/8cd878b::delete_forest2[0]")]
unsafe fn delete_forest2(forest: *mut Forest) {
    let t: *mut Tree = head_take(forest);
    delete_tree2(t);
    let f: *mut Forest = tail_take(forest);
    if !f.is_null() {
        delete_forest2(f);
    }
    free(forest as *mut c_void);
}


#[ownership_mono("")]
#[ownership_variant_of("analysis_ownership/8cd878b::main[0]")]
fn main() {}
