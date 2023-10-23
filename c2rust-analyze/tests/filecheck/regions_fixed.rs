#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]

use std::ptr;

// CHECK-LABEL: struct WrapPtr<'h0>
struct WrapPtr {
    // CHECK: x: *mut u8,
    #[c2rust_analyze_test::fixed_signature]
    x: *mut u8,
    // CHECK: y: &'h0 mut (u8),
    y: *mut u8,
}

// CHECK-LABEL: struct UseWrapPtr<'h0,'h1>
struct UseWrapPtr {
    // CHECK: p: *mut WrapPtr<'h0>,
    #[c2rust_analyze_test::fixed_signature]
    p: *mut WrapPtr,
    // CHECK: q: &'h1 (WrapPtr<'h0>),
    q: *mut WrapPtr,
}

// CHECK-LABEL: fn f_ptr<'h0>(x: &'h0 mut (u8))
unsafe fn f_ptr(x: *mut u8) {
    *x = 1;
}

// FIXME: regression: missing lifetime in struct; the correct rewrite is:
// XXXXX-LABEL: fn f_wrap_ptr<'h0,'h1>(p: &'h0 (WrapPtr<'h1>))
// CHECK-LABEL: fn f_wrap_ptr<'h0,'h1>(p: &'h0 WrapPtr)
unsafe fn f_wrap_ptr(p: *mut WrapPtr) {
    f_ptr((*p).x);
    f_ptr((*p).y);
}

// FIXME: regression: missing lifetime in struct; the correct rewrite is:
// XXXXX-LABEL: fn f_use_wrap_ptr<'h0,'h1,'h2>(u: &'h0 (UseWrapPtr<'h1,'h2>))
// CHECK-LABEL: fn f_use_wrap_ptr<'h0,'h1,'h2>(u: &'h0 UseWrapPtr)
unsafe fn f_use_wrap_ptr(u: *mut UseWrapPtr) {
    f_wrap_ptr((*u).p);
    f_wrap_ptr((*u).q);
}
