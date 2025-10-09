use std::ptr;

// CHECK-LABEL: final labeling for "alias3_copy_bad1"
// CHECK-DAG: ([[@LINE+2]]: x): {{.*}}type = READ | WRITE | NON_NULL | HEAP | STACK#
// CHECK-DAG: ([[@LINE+1]]: x): {{.*}}type flags = CELL#
pub unsafe fn alias3_copy_bad1(x: *mut i32) {
    // CHECK-DAG: ([[@LINE+2]]: p): {{.*}}type = READ | NON_NULL | HEAP | STACK#
    // CHECK-DAG: ([[@LINE+1]]: p): {{.*}}type flags = CELL#
    let p = x;
    // CHECK-DAG: ([[@LINE+2]]: q): {{.*}}type = READ | WRITE | NON_NULL | HEAP | STACK#
    // CHECK-DAG: ([[@LINE+1]]: q): {{.*}}type flags = CELL#
    let q = x;
    *q = *p;
}

// CHECK-LABEL: final labeling for "alias3_copy_bad2"
// CHECK-DAG: ([[@LINE+2]]: x): {{.*}}type = READ | WRITE | NON_NULL | HEAP | STACK#
// CHECK-DAG: ([[@LINE+1]]: x): {{.*}}type flags = CELL#
pub unsafe fn alias3_copy_bad2(x: *mut i32) {
    // CHECK-DAG: ([[@LINE+2]]: p): {{.*}}type = READ | WRITE | NON_NULL | HEAP | STACK#
    // CHECK-DAG: ([[@LINE+1]]: p): {{.*}}type flags = CELL#
    let p = x;
    // CHECK-DAG: ([[@LINE+2]]: q): {{.*}}type = READ | NON_NULL | HEAP | STACK#
    // CHECK-DAG: ([[@LINE+1]]: q): {{.*}}type flags = CELL#
    let q = x;
    *p = *q;
}

#[cfg(debug_polonius_facts)]
pub unsafe fn alias3_addr_of_bad1(x: *mut i32) {
    let p = ptr::addr_of_mut!(*x);
    let q = ptr::addr_of_mut!(*x);
    *q = *p;
}

#[cfg(debug_polonius_facts)]
pub unsafe fn alias3_addr_of_bad2(x: *mut i32) {
    let p = ptr::addr_of_mut!(*x);
    let q = ptr::addr_of_mut!(*x);
    *p = *q;
}
