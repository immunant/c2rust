// CHECK-DAG: ([[@LINE+1]]: x): &[i32]
pub unsafe fn slice_as_ptr_load(x: &[i32]) -> i32 {
    // CHECK-DAG: ([[@LINE+1]]: p): &i32
    let p = x.as_ptr();
    *p
}

// FIXME: currently misinferred as &[[i32; 10]]
// COM: CHECK-DAG: ([[@LINE+1]]: x): &[i32]
pub unsafe fn slice_as_ptr_offset_load(x: &[i32]) -> i32 {
    // CHECK-DAG: ([[@LINE+1]]: p): &[i32]
    let p = x.as_ptr();
    // CHECK-DAG: ([[@LINE+1]]: q): &i32
    let q = p.offset(2);
    *q
}

// CHECK-DAG: ([[@LINE+1]]: x): &[i32; 10]
pub unsafe fn array_as_ptr_load(x: &[i32; 10]) -> i32 {
    // CHECK-DAG: ([[@LINE+1]]: p): &i32
    let p = x.as_ptr();
    *p
}

// FIXME: currently misinferred as &[[i32; 10]]
// COM: CHECK-DAG: ([[@LINE+1]]: x): &[i32; 10]
pub unsafe fn array_as_ptr_offset_load(x: &[i32; 10]) -> i32 {
    // CHECK-DAG: ([[@LINE+1]]: p): &[i32]
    let p = x.as_ptr();
    // CHECK-DAG: ([[@LINE+1]]: q): &i32
    let q = p.offset(2);
    *q
}
