// CHECK-DAG: struct S<'h0> {
struct S {
    // CHECK-DAG: i: &'h0 i32
    i: *const i32,
}

// The below ensures the concrete origins for `s` and `s.i` are the same and are hypothetical
// CHECK-DAG: assign {{.*}}#*mut S{{.*}}origin_params: [('h0, Origin([[HYPO_ORIGIN:[0-9]+]]))]{{.*}} = Label{{.*}}origin_params: [('h0, Origin({{.*}}))]
// CHECK-DAG: assign Label { origin: Some(Origin([[HYPO_ORIGIN]])){{.*}}*const i32{{.*}} = Label

// CHECK-LABEL: final labeling for "null_ptr"
pub unsafe fn null_ptr() {
    // CHECK-DAG: ([[@LINE+3]]: s): addr_of = UNIQUE, type = READ | WRITE | UNIQUE#
    // CHECK-LABEL: type assignment for "null_ptr":
    // CHECK-DAG: ([[@LINE+1]]: s): &mut S
    let s = 0 as *mut S;
    (*s).i = 0 as *const i32;
}
