#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]
// Tests the insertion of casts at call sites.

// CHECK-LABEL: fn use_single(x: *mut i32)
#[c2rust_analyze_test::fixed_signature]
#[c2rust_analyze_test::skip_rewrite]
unsafe fn use_single(x: *mut i32) {
    *x = 1;
}

// CHECK-LABEL: fn use_slice(x: *mut i32)
#[c2rust_analyze_test::fixed_signature]
#[c2rust_analyze_test::skip_rewrite]
unsafe fn use_slice(x: *mut i32) {
    *x.offset(1) = 1;
}


// CHECK-LABEL: fn f(x: &mut (i32))
unsafe fn f(x: *mut i32) {
    // CHECK: use_single(core::ptr::addr_of_mut!(*(x)))
    use_single(x);
}

// CHECK-LABEL: fn g(x: &mut [(i32)])
unsafe fn g(x: *mut i32) {
    // CHECK: use_single(core::ptr::addr_of_mut!(*&mut (x)[0]))
    use_single(x);

    // CHECK: use_slice(core::ptr::addr_of_mut!(*&mut (x)[0]))
    use_slice(x);
}
