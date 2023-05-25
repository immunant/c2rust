#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]
// Test calls between rewritten and non-rewritten functions.

// CHECK-LABEL: fn f(x: &mut (i32))
unsafe fn f(x: *mut i32) -> i32 {
    // CHECK: let y = &*(g(core::ptr::addr_of_mut!(*(x)))).cast_const();
    let y = g(x);
    *y
}

// The middle function, `g`, will be marked as failed.  We wait until analysis has finished before
// marking it so that the `WRITE` requirement from `h` will be propagated up to `f`.
#[c2rust_analyze_test::fail_before_rewriting]
// CHECK-LABEL: fn g(x: *mut i32) -> *mut i32
unsafe fn g(x: *mut i32) -> *mut i32 {
    // CHECK: h_shim(x)
    h(x)
}

// CHECK-LABEL: fn h(x: &mut (i32)) -> &(i32)
unsafe fn h(x: *mut i32) -> *mut i32 {
    *x = 1;
    // CHECK: &*(x)
    x
}
// CHECK: fn h_shim(arg0: *mut i32) -> *mut i32
// CHECK: core::ptr::addr_of!(*h(&mut *arg0)).cast_mut()
