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

// Analysis of `call_bad` should also fail because it has a callee on which analysis failed.
// CHECK-NOT: final labeling for "call_bad"
unsafe fn call_bad(p: NonNull<u8>) {
    bad(p);
}

// Analysis of this function fails, but it also calls `good`.  Failures should not propagate from
// caller to callee (only callee to caller), so analysis of `good` should still succeed.
// CHECK-NOT: final labeling for "bad_call_good"
unsafe fn bad_call_good(p: NonNull<u8>) {
    *p.as_ptr() = 1;
    good(p.as_ptr());
}

// CHECK: analysis of DefId({{.*}}::bad) failed:
// CHECK-SAME: UnknownDef
// CHECK-SAME: NonNull::<u8>::as_ptr

// CHECK: analysis of DefId({{.*}}::call_bad) failed:
// CHECK-SAME: analysis failed on callee DefId({{.*}}::bad)

// CHECK: analysis of DefId({{.*}}::bad_call_good) failed:
// CHECK-SAME: UnknownDef
// CHECK-SAME: NonNull::<u8>::as_ptr

// CHECK: saw errors in 3 / 4 functions
