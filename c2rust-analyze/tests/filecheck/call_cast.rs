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

// CHECK-LABEL: fn pass_single(x: *mut i32) -> *mut i32
#[c2rust_analyze_test::fixed_signature]
#[c2rust_analyze_test::skip_rewrite]
unsafe fn pass_single(x: *mut i32) -> *mut i32 {
    use_single(x);
    x
}

// CHECK-LABEL: fn pass_slice(x: *mut i32) -> *mut i32
#[c2rust_analyze_test::fixed_signature]
#[c2rust_analyze_test::skip_rewrite]
unsafe fn pass_slice(x: *mut i32) -> *mut i32 {
    use_slice(x);
    x
}


// CHECK-LABEL: fn f(x: &mut (i32))
unsafe fn f(x: *mut i32) {
    // CHECK: use_single(core::ptr::addr_of_mut!(*(x)))
    use_single(x);

    // CHECK: let y: &mut (i32)
    // CHECK-SAME: pass_single(core::ptr::addr_of_mut!(*(x)))
    let y: *mut i32 = pass_single(x);
    use_single(y);
}

// CHECK-LABEL: fn g(x: &mut [(i32)])
unsafe fn g(x: *mut i32) {
    // CHECK: use_single(core::ptr::addr_of_mut!(*&mut (x)[0]))
    use_single(x);

    // CHECK: use_slice(core::ptr::addr_of_mut!(*&mut (x)[0]))
    use_slice(x);

    // CHECK: let y: &mut (i32)
    // CHECK-SAME: &mut *(pass_single(core::ptr::addr_of_mut!(*&mut (x)[0])))
    let y: *mut i32 = pass_single(x);
    // CHECK: use_single(core::ptr::addr_of_mut!(*(y)))
    use_single(y);

    // Currently impossible: this would require a cast from `*mut i32` to `&mut [i32]`, but we
    // don't have any way to obtain the slice length.
    /*
    let z: *mut i32 = pass_slice(x);
    use_slice(z);
    */
}
