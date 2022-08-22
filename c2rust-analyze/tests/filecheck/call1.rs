// CHECK-LABEL: final labeling for "call1"
// CHECK-DAG: ([[#@LINE+1]]: x): &mut i32
pub unsafe fn call1(x: *mut i32) {
    // CHECK-DAG: ([[#@LINE+1]]: p): &mut i32
    let p = x;
    write(p);
    // CHECK-DAG: ([[#@LINE+1]]: q): &i32
    let q = x;
    let y = read(q);
}

pub unsafe fn call2(x: *mut i32) {
    // CHECK-DAG: ([[#@LINE+1]]: p): &mut i32
    let p = x;
    write(p);
    // CHECK-DAG: ([[#@LINE+1]]: q): &mut i32
    let q = x;
    non_unique(q);
}

// CHECK-LABEL: final labeling for "write"
// CHECK-DAG: ([[#@LINE+1]]: x): &mut i32
unsafe fn write(x: *mut i32) {
    *x = 1;
}

// CHECK-LABEL: final labeling for "read"
// CHECK-DAG: ([[#@LINE+1]]: x): &i32
unsafe fn read(x: *mut i32) -> i32 {
    *x
}

// CHECK-LABEL: final labeling for "non_unique"
// CHECK-DAG: ([[#@LINE+1]]: x): &std::cell::Cell<i32>
unsafe fn non_unique(x: *mut i32) {
    let y = x;
    *y = 1;
    *x = 2;
    *y = 3;
    *x = 4;
}
