use std::ptr;

// CHECK-LABEL: final labeling for "f"
fn f(cond: bool) {
    let x = 1_i32;
    // CHECK: ([[@LINE+1]]: mut y): {{.*}}, type = UNIQUE | STACK#
    let mut y = ptr::addr_of!(x);
    if cond {
        y = ptr::null();
    }
}

// CHECK-LABEL: final labeling for "g"
fn g(cond: bool) {
    let x = 1_i32;
    // CHECK: ([[@LINE+1]]: y): {{.*}}, type = UNIQUE | NON_NULL | STACK#
    let y = ptr::addr_of!(x);
    if cond {
        let z = ptr::null::<i32>();
    }
}

// CHECK-LABEL: final labeling for "h"
fn h(cond: bool) {
    let x = 1_i32;
    // CHECK: ([[@LINE+1]]: y): {{.*}}, type = READ | UNIQUE | NON_NULL | STACK#
    let y = ptr::addr_of!(x);
    // CHECK: ([[@LINE+1]]: z): {{.*}}, type = UNIQUE | STACK#
    let z = if cond {
        y
    } else {
        ptr::null()
    };
}


// Like `f`, but uses `0 as *const _` instead of `ptr::null()`.
// CHECK-LABEL: final labeling for "f_zero"
fn f_zero(cond: bool) {
    let x = 1_i32;
    // CHECK: ([[@LINE+1]]: mut y): {{.*}}, type = UNIQUE | STACK#
    let mut y = ptr::addr_of!(x);
    if cond {
        y = 0 as *const _;
    }
}
