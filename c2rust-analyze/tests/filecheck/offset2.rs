use std::ptr;

// CHECK-LABEL: final labeling for "offset2_good"
// CHECK-DAG: ([[#@LINE+1]]: x): {{.*}}type = READ | WRITE | UNIQUE | OFFSET_ADD | OFFSET_SUB#
pub unsafe fn offset2_good(x: *mut i32, off: isize) {
    // CHECK-DAG: ([[#@LINE+2]]: x): {{.*}}type = UNIQUE | OFFSET_ADD | OFFSET_SUB#
    // CHECK-DAG: ([[#@LINE+1]]: p): {{.*}}type = UNIQUE#
    let p = x.offset(off);
    // CHECK-DAG: ([[#@LINE+2]]: x): {{.*}}type = READ | WRITE | UNIQUE | OFFSET_ADD | OFFSET_SUB#
    // CHECK-DAG: ([[#@LINE+1]]: q): {{.*}}type = READ | WRITE | UNIQUE#
    let q = x.offset(off);
    *q = 1;
}

// CHECK-LABEL: final labeling for "offset2_bad"
// CHECK-DAG: ([[#@LINE+1]]: x): {{.*}}type = READ | WRITE | OFFSET_ADD | OFFSET_SUB#
pub unsafe fn offset2_bad(x: *mut i32, off: isize) {
    // CHECK-DAG: ([[#@LINE+2]]: x): {{.*}}type = READ | WRITE | OFFSET_ADD | OFFSET_SUB#
    // CHECK-DAG: ([[#@LINE+1]]: p): {{.*}}type = READ | WRITE#
    let p = x.offset(off);
    // CHECK-DAG: ([[#@LINE+2]]: x): {{.*}}type = OFFSET_ADD | OFFSET_SUB#
    // CHECK-DAG: ([[#@LINE+1]]: q): {{.*}}type = (empty)#
    let q = x.offset(off);
    *p = 1;
}
