// CHECK-LABEL: final labeling for "slice_as_ptr_load"
// CHECK-LABEL: type assignment for "slice_as_ptr_load"
// CHECK-DAG: ([[@LINE+1]]: x): &[i32]
pub unsafe fn slice_as_ptr_load(x: &[i32]) -> i32 {
    // CHECK-DAG: ([[@LINE+1]]: p): &i32
    let p = x.as_ptr();
    *p
}

// CHECK-LABEL: final labeling for "slice_as_ptr_offset_load"
// CHECK-LABEL: type assignment for "slice_as_ptr_offset_load"
// CHECK-DAG: ([[@LINE+1]]: x): &{{\[i32]}}
pub unsafe fn slice_as_ptr_offset_load(x: &[i32]) -> i32 {
    // CHECK-DAG: ([[@LINE+1]]: p): &[i32]
    let p = x.as_ptr();
    // CHECK-DAG: ([[@LINE+1]]: q): &i32
    let q = p.offset(2);
    *q
}

// CHECK-LABEL: final labeling for "array_as_ptr_load"
// CHECK-LABEL: type assignment for "array_as_ptr_load"
// CHECK-DAG: ([[@LINE+1]]: x): &[i32; 10]
pub unsafe fn array_as_ptr_load(x: &[i32; 10]) -> i32 {
    // CHECK-DAG: ([[@LINE+1]]: p): &i32
    let p = x.as_ptr();
    *p
}

// CHECK-LABEL: final labeling for "array_as_ptr_offset_load"
// CHECK-LABEL: type assignment for "array_as_ptr_offset_load"
// CHECK-DAG: ([[@LINE+1]]: x): &{{\[i32; 10]}}
pub unsafe fn array_as_ptr_offset_load(x: &[i32; 10]) -> i32 {
    // CHECK-DAG: ([[@LINE+1]]: p): &[i32]
    let p = x.as_ptr();
    // CHECK-DAG: ([[@LINE+1]]: q): &i32
    let q = p.offset(2);
    *q
}
