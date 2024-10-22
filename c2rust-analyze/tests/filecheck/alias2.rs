use std::ptr;

// CHECK-LABEL: final labeling for "alias2_copy_good"
// CHECK-DAG: ([[@LINE+1]]: x): {{.*}}type = READ | WRITE | UNIQUE | NON_NULL | HEAP | STACK#
pub unsafe fn alias2_copy_good(x: *mut i32) {
    // CHECK-DAG: ([[@LINE+1]]: p): {{.*}}type = UNIQUE | NON_NULL | HEAP | STACK#
    let p = x;
    // CHECK-DAG: ([[@LINE+1]]: q): {{.*}}type = READ | WRITE | UNIQUE | NON_NULL | HEAP | STACK#
    let q = x;
    *q = 1;
}

// CHECK-LABEL: final labeling for "alias2_addr_of_good"
// CHECK-DAG: ([[@LINE+1]]: x): {{.*}}type = READ | WRITE | UNIQUE | NON_NULL | HEAP | STACK#
pub unsafe fn alias2_addr_of_good(x: *mut i32) {
    // CHECK-DAG: ([[@LINE+1]]: p): {{.*}}type = UNIQUE | NON_NULL | HEAP | STACK#
    let p = ptr::addr_of_mut!(*x);
    // CHECK-DAG: ([[@LINE+1]]: q): {{.*}}type = READ | WRITE | UNIQUE | NON_NULL | HEAP | STACK#
    let q = ptr::addr_of_mut!(*x);
    *q = 1;
}

// CHECK-LABEL: final labeling for "alias2_copy_bad"
// CHECK-DAG: ([[@LINE+2]]: x): {{.*}}type = READ | WRITE | NON_NULL | HEAP | STACK#
// CHECK-DAG: ([[@LINE+1]]: x): {{.*}}type flags = CELL#
pub unsafe fn alias2_copy_bad(x: *mut i32) {
    // CHECK-DAG: ([[@LINE+1]]: p): {{.*}}type = READ | WRITE | NON_NULL | HEAP | STACK#
    let p = x;
    // CHECK-DAG: ([[@LINE+1]]: q): {{.*}}type = NON_NULL | HEAP | STACK#
    let q = x;
    *p = 1;
}

// CHECK-LABEL: final labeling for "alias2_addr_of_bad"
// CHECK-DAG: ([[@LINE+2]]: x): {{.*}}type = READ | WRITE | NON_NULL | HEAP | STACK#
// CHECK-DAG: ([[@LINE+1]]: x): {{.*}}type flags = CELL#
pub unsafe fn alias2_addr_of_bad(x: *mut i32) {
    // CHECK-DAG: ([[@LINE+1]]: p): {{.*}}type = READ | WRITE | NON_NULL | HEAP | STACK#
    let p = ptr::addr_of_mut!(*x);
    // CHECK-DAG: ([[@LINE+1]]: q): {{.*}}type = NON_NULL | HEAP | STACK#
    let q = ptr::addr_of_mut!(*x);
    *p = 1;
}


#[cfg(debug_polonius_facts)]
pub unsafe fn safe_alias2_copy_good(x: &mut i32) {
    let p = x;
    let q = x;
    *q = 1;
}

#[cfg(debug_polonius_facts)]
pub unsafe fn safe_alias2_addr_of_good(x: &mut i32) {
    let p = &mut *x;
    let q = &mut *x;
    *q = 1;
}

#[cfg(debug_polonius_facts)]
pub unsafe fn safe_alias2_copy_bad(x: &mut i32) {
    let p = x;
    let q = x;
    *p = 1;
}

#[cfg(debug_polonius_facts)]
pub unsafe fn safe_alias2_addr_of_bad(x: &mut i32) {
    let p = &mut *x;
    let q = &mut *x;
    *p = 1;
}
