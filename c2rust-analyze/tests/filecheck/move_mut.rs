use std::ptr;

// CHECK-LABEL: final labeling for "move_mut1"
pub unsafe fn move_mut1(mut x: i32) -> i32 {
    // CHECK-DAG: ([[@LINE+1]]: q): addr_of = UNIQUE | NON_NULL | STACK, type = READ | WRITE | UNIQUE | NON_NULL | STACK#
    let q: *mut i32 = {
        // CHECK-DAG: ([[@LINE+1]]: p): addr_of = UNIQUE | NON_NULL | STACK, type = READ | WRITE | UNIQUE | NON_NULL | STACK#
        let p: *mut i32 = ptr::addr_of_mut!(x);
        *p = 1;
        // This produces a move from `p` into `q`.  c2rust-analyze interprets this as a reborrow,
        // which gets invalidated at the end of `p`'s lifetime.  Currently we work around this by
        // ignoring `StorageLive` and `StorageDead`, so they don't invalidate any borrows.
        p
    };
    *q = 2;
    *q
}

// Safe version of `move_mut1`.  This can be run through `rustc -Z nll-facts` to produce Polonius
// facts from the reference implementation for comparison.
// CHECK-LABEL: final labeling for "move_mut1_safe"
pub fn move_mut1_safe(mut x: i32) -> i32 {
    let q: &mut i32 = {
        let p: &mut i32 = &mut x;
        *p = 1;
        p
    };
    *q = 2;
    *q
}
