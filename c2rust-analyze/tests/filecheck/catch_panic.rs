// This test deliberately exercises unsupported code to check that c2rust-analyze can recover from
// panics during the analysis.

use std::ptr::NonNull;

// CHECK: final labeling for "good"
unsafe fn good(p: *mut u8) {
    *p = 1;
}

// Analysis of `bad` should fail because it calls an unsupported library function involving raw
// pointers, namely `NonNull::as_ptr`.
// CHECK-NOT: final labeling for "bad"
unsafe fn bad(p: NonNull<u8>) {
    *p.as_ptr() = 1;
}

// Analysis of `bad2` should also fail because it has a callee on which analysis failed.
// CHECK-NOT: final labeling for "bad2"
unsafe fn bad2(p: NonNull<u8>) {
    bad(p);
}

// CHECK: analysis of DefId({{.*}}::bad) failed:
// CHECK-SAME: UnknownDef
// CHECK-SAME: NonNull::<u8>::as_ptr

// CHECK: analysis of DefId({{.*}}::bad2) failed:
// CHECK-SAME: analysis failed on callee DefId({{.*}}::bad)
