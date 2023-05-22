// With a naive iteration order, reaching a fixpoint used to take 3 iterations.  By following a
// postorder traversal of the callgraph, we reduce that to 2 iterations: the first iteration
// computes all the right permissions, and the second checks that we've actually reached a
// fixpoint.
//
// CHECK: reached fixpoint in 2 iterations

// CHECK-LABEL: final labeling for "call1"
// CHECK-DAG: ([[@LINE+1]]: x): &mut i32
pub unsafe fn call1(x: *mut i32) {
    // CHECK-DAG: ([[@LINE+1]]: p): &mut i32
    let p = x;
    write(p);
    // CHECK-DAG: ([[@LINE+1]]: q): &i32
    let q = x;
    let y = read(q);
}

// CHECK-LABEL: final labeling for "call2"
pub unsafe fn call2(x: *mut i32) {
    // CHECK-DAG: ([[@LINE+1]]: p): &mut i32
    let p = x;
    write(p);
    // CHECK-DAG: ([[@LINE+1]]: q): &mut i32
    let q = x;
    non_unique(q);
}

// CHECK-LABEL: final labeling for "write"
// CHECK-DAG: ([[@LINE+1]]: x): &mut i32
unsafe fn write(x: *mut i32) {
    *x = 1;
}

// CHECK-LABEL: final labeling for "read"
// CHECK-DAG: ([[@LINE+1]]: x): &i32
unsafe fn read(x: *mut i32) -> i32 {
    *x
}

// CHECK-LABEL: final labeling for "non_unique"
// CHECK-DAG: ([[@LINE+1]]: x): &std::cell::Cell<i32>
unsafe fn non_unique(x: *mut i32) {
    let y = x;
    *y = 1;
    *x = 2;
    *y = 3;
    *x = 4;
}
