#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]

use std::ptr;

// CHECK-LABEL: final labeling for "f"
fn f(cond: bool) {
    let x = 1_i32;
    // CHECK: ([[@LINE+1]]: mut y): {{.*}}, type = READ | UNIQUE | STACK#
    let mut y = ptr::addr_of!(x);
    if cond {
        y = 0 as *const _;
    }
    // The expression `y` is considered nullable even though it's passed for argument `p` of `g`,
    // which is forced to be `NON_NULL`.
    // CHECK: ([[@LINE+1]]: y): {{.*}}, type = READ | UNIQUE | STACK#
    g(cond, y);
}

// CHECK-LABEL: final labeling for "g"
// `p` should be non-null, as it's forced to be by the attribute.  This emulates the "unsound" PDG
// case, where a variable is forced to stay `NON_NULL` even though a null possibly flows into it.
// CHECK: ([[@LINE+2]]: p): {{.*}}, type = READ | UNIQUE | NON_NULL | STACK#
#[c2rust_analyze_test::force_non_null_args]
fn g(cond: bool, p: *const i32) {
    // `q` is not forced to be `NON_NULL`, so it should be inferred nullable due to the null
    // assignment below.
    // CHECK: ([[@LINE+1]]: mut q): {{.*}}, type = READ | UNIQUE | STACK#
    let mut q = p;
    if cond {
        q = 0 as *const _;
    }
    // `r` is derived from `q` (and is not forced), so it should also be nullable.
    // CHECK: ([[@LINE+1]]: r): {{.*}}, type = UNIQUE | STACK#
    let r = q;
}

