//! --rewrite-paths good1,good2
#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]

// CHECK-LABEL: fn good1<'h0>(x: &'h0 mut (i32))
unsafe fn good1(x: *mut i32) -> i32 {
    // CHECK: let y = &*(bad(core::ptr::addr_of_mut!(*(x)))).cast_const();
    let y = bad(x);
    *y
}

// CHECK-LABEL: fn bad(x: *mut i32) -> *mut i32
// Since `bad` is not listed in --rewrite-paths, it will not be rewritten (aside from adding shim
// calls).
unsafe fn bad(x: *mut i32) -> *mut i32 {
    // CHECK: good2_shim(x)
    good2(x)
}

// CHECK-LABEL: fn good2<'h0,'h1>(x: &'h0 mut (i32)) -> &'h1 (i32)
unsafe fn good2(x: *mut i32) -> *mut i32 {
    *x = 1;
    // CHECK: &*(x)
    x
}
// CHECK-LABEL: unsafe fn good2_shim(arg0: *mut i32) -> *mut i32
// CHECK: let safe_arg0 = &mut *arg0;
// CHECK: let safe_result = good2(safe_arg0);
// CHECK: let result = core::ptr::addr_of!(*safe_result).cast_mut();

// Regression test for an issue where `gen_shim_call_rewrites` would panic upon encountering a
// non-rewritten function that doesn't have a body.
extern "C" {
    fn memcpy(_: *mut u8, _: *const u8, _: usize) -> *mut u8;
}
