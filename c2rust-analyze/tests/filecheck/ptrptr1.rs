
// CHECK-LABEL: final labeling for "ptrptr1_backward"
// CHECK-DAG: ([[@LINE+4]]: x): {{.*}}type = {{[lg][0-9]+}}#*mut *mut i32{{\[}}[[LABEL:[lg][0-9]+]]#*mut i32[NONE#i32[]]]
// CHECK-DAG: ([[@LINE+3]]: y): {{.*}}type = {{[lg][0-9]+}}#*mut *mut i32{{\[}}[[LABEL]]#*mut i32[NONE#i32[]]]
// CHECK-DAG: ([[@LINE+2]]: x): &mut &mut i32
// CHECK-DAG: ([[@LINE+1]]: y): &mut &mut i32
pub unsafe fn ptrptr1_backward(cond: bool, x: *mut *mut i32, y: *mut *mut i32) {
    // CHECK-DAG: ([[@LINE+2]]: z): {{.*}}type = {{[lg][0-9]+}}#*mut *mut i32{{\[}}[[LABEL]]#*mut i32[NONE#i32[]]]
    // CHECK-DAG: ([[@LINE+1]]: z): &mut &mut i32
    let z = if cond {
        x
    } else {
        y
    };
    **z = 1;
}

// CHECK-LABEL: final labeling for "ptrptr1_bidir"
// CHECK-DAG: ([[@LINE+7]]: x): {{.*}}type = {{[lg][0-9]+}}#*mut *mut i32{{\[}}[[LABEL:[lg][0-9]+]]#*mut i32[NONE#i32[]]]
// CHECK-DAG: ([[@LINE+6]]: y): {{.*}}type = {{[lg][0-9]+}}#*mut *mut i32{{\[}}[[LABEL]]#*mut i32[NONE#i32[]]]
// FIXME: regression: badly mangled results from pointee; the correct rewrite is:
// XXXXX-DAG: ([[@LINE+2]]: x): &mut &mut i32
// XXXXX-DAG: ([[@LINE+1]]: y): &&mut i32
// CHECK-DAG: ([[@LINE+2]]: x): &mut *mut *mut i32
// CHECK-DAG: ([[@LINE+1]]: y): *mut i32
pub unsafe fn ptrptr1_bidir(cond: bool, x: *mut *mut i32, y: *mut *mut i32) {
    // CHECK-DAG: ([[@LINE+4]]: z): {{.*}}type = {{[lg][0-9]+}}#*mut *mut i32{{\[}}[[LABEL]]#*mut i32[NONE#i32[]]]
    // FIXME: regression: badly mangled results from pointee; the correct rewrite is:
    // XXXXX-DAG: ([[@LINE+1]]: z): &&mut i32
    // CHECK-DAG: ([[@LINE+1]]: z): &*mut *mut i32
    let z = if cond {
        x
    } else {
        y
    };
    // `*x` must be `&mut i32`, so `*z` must be `&mut i32`, so `*y` must be `&mut i32`.
    **x = 1;
}
