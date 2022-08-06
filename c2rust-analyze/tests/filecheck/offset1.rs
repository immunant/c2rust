use std::ptr;

// CHECK-LABEL: final labeling for "offset1_const"
// CHECK-DAG: ([[#@LINE+1]]: x): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB#
pub unsafe fn offset1_const(x: *mut i32) -> i32 {
    // CHECK-DAG: ([[#@LINE+2]]: x): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB#
    // CHECK-DAG: ([[#@LINE+1]]: x.offset(1)): {{.*}}type = READ | UNIQUE#
    *x.offset(1)
}

// CHECK-LABEL: final labeling for "offset1_unknown"
// CHECK-DAG: ([[#@LINE+1]]: x): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB#
pub unsafe fn offset1_unknown(x: *mut i32, off: isize) -> i32 {
    // CHECK-DAG: ([[#@LINE+2]]: x): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB#
    // CHECK-DAG: ([[#@LINE+1]]: x.offset(off)): {{.*}}type = READ | UNIQUE#
    *x.offset(off)
}

/*
pub unsafe fn offset1_usize(x: *mut i32, off: usize) -> i32 {
    *x.offset(off as isize)
}
*/

// CHECK-LABEL: final labeling for "offset1_immut"
// CHECK-DAG: ([[#@LINE+1]]: x): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB#
pub unsafe fn offset1_immut(x: *const i32, off: isize) -> i32 {
    // CHECK-DAG: ([[#@LINE+2]]: x): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB#
    // CHECK-DAG: ([[#@LINE+1]]: x.offset(off)): {{.*}}type = READ | UNIQUE#
    *x.offset(off)
}

// CHECK-LABEL: final labeling for "offset1_double"
// CHECK-DAG: ([[#@LINE+1]]: x): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB#
pub unsafe fn offset1_double(x: *mut i32, off: isize) -> i32 {
    // CHECK-DAG: ([[#@LINE+3]]: x): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB#
    // CHECK-DAG: ([[#@LINE+2]]: x.offset(off)): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB#
    // CHECK-DAG: ([[#@LINE+1]]: x.offset{{.*}}...{{.*}}): {{.*}}type = READ | UNIQUE#
    *x.offset(off).offset(off)
}
