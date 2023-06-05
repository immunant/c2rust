#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]

// CHECK-LABEL: type assignment for "f":
// CHECK: _0 ({{.*}}: *mut i32): *mut i32
// CHECK: _1 ({{.*}}: x): *mut i32
#[c2rust_analyze_test::fixed_signature]
fn f(x: *mut i32) -> *mut i32 {
    x
}

// CHECK-LABEL: type assignment for "g":
// CHECK: _0 ({{.*}}: *mut i32): &i32
// CHECK: _1 ({{.*}}: x): &i32
#[c2rust_analyze_test::skip_rewrite]
fn g(x: *mut i32) -> *mut i32 {
    x
}

// CHECK: analysis of DefId({{.*}} ~ test_attrs[{{.*}}]::h) failed: [unknown]: explicit fail_analysis for testing
#[c2rust_analyze_test::fail_analysis]
fn h(x: *mut i32) -> *mut i32 {
    x
}
