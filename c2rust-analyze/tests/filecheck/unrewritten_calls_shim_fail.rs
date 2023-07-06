//! --catch-panics
#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]
// Like `unrewritten_calls.rs`, but specifically testing the case where shim generation fails.

// CHECK-LABEL: fn slice_good1(x: &mut [(i32)])
unsafe fn slice_good1(x: *mut i32) -> i32 {
    let y = slice_bad(x);
    *y
}

// CHECK-LABEL: fn slice_bad(x: *mut i32) -> *mut i32
#[c2rust_analyze_test::fail_before_rewriting]
unsafe fn slice_bad(x: *mut i32) -> *mut i32 {
    // Since shim generation for `slice_use_offset` fails, after recovery there should be no shim
    // call here.
    // CHECK: slice_use_offset(x)
    slice_use_offset(x)
}

// Shim generation fails on this function due to the unsupported `*mut i32 -> &mut [i32]` cast for
// the argument.
// CHECK-LABEL: fn slice_use_offset(x: *mut i32) -> *mut i32
unsafe fn slice_use_offset(x: *mut i32) -> *mut i32 {
    *x.offset(1) = 1;
    // After `slice_use_offset` is marked failed, this will be changed into a shim call.
    // CHECK: slice_good2_shim(x)
    slice_good2(x);
    x
}

// CHECK-LABEL: fn slice_good2(x: &mut (i32)) -> &(i32)
unsafe fn slice_good2(x: *mut i32) -> *mut i32 {
    *x = 1;
    x
}
// CHECK: unsafe fn slice_good2_shim(arg0: *mut i32) -> *mut i32
