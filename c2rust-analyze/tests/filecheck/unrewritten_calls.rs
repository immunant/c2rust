#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]
// Test calls between rewritten and non-rewritten functions.

// CHECK-LABEL: fn good1(x: &mut (i32))
unsafe fn good1(x: *mut i32) -> i32 {
    // CHECK: let y = &*(bad(core::ptr::addr_of_mut!(*(x)))).cast_const();
    let y = bad(x);
    *y
}

// The middle function, `bad`, will be marked as failed.  We wait until analysis has finished
// before marking it so that the `WRITE` requirement from `good2` will be propagated up to `good1`.
#[c2rust_analyze_test::fail_before_rewriting]
// CHECK-LABEL: fn bad(x: *mut i32) -> *mut i32
unsafe fn bad(x: *mut i32) -> *mut i32 {
    // CHECK: good2_shim(x)
    good2(x)
}

// CHECK-LABEL: fn good2(x: &mut (i32)) -> &(i32)
unsafe fn good2(x: *mut i32) -> *mut i32 {
    *x = 1;
    // CHECK: &*(x)
    x
}
// CHECK: unsafe fn good2_shim(arg0: *mut i32) -> *mut i32
// CHECK: core::ptr::addr_of!(*good2(&mut *arg0)).cast_mut()
