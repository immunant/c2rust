
// CHECK-LABEL: final labeling for "ptrptr1_basic"
// CHECK-DAG: ([[#@LINE+2]]: x): {{.*}}type = {{[lg][0-9]+}}#*mut *mut i32{{\[}}[[LABEL:[lg][0-9]+]]#*mut i32[NONE#i32[]]]
// CHECK-DAG: ([[#@LINE+1]]: y): {{.*}}type = {{[lg][0-9]+}}#*mut *mut i32{{\[}}[[LABEL]]#*mut i32[NONE#i32[]]]
pub unsafe fn ptrptr1_basic(cond: bool, x: *mut *mut i32, y: *mut *mut i32) {
    // CHECK-DAG: ([[#@LINE+1]]: z): {{.*}}type = {{[lg][0-9]+}}#*mut *mut i32{{\[}}[[LABEL]]#*mut i32[NONE#i32[]]]
    let z = if cond {
        x
    } else {
        y
    };
    **z = 1;
}
