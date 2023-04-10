struct S<'a> {
    y: u32,
    px: *const u32,
    rx: &'a u32,
    qx: &'a u32,
}

pub fn aggregate() {
    // CHECK-DAG: pseudo-assigning fields Label { origin: Some(Origin([[R_U32_ORIGIN:[0-9]+]])){{.*}}}#&'a u32{{.*}}= Label { origin: Some(Origin({{.*}})){{.*}}&u32
    // CHECK-DAG: pseudo-assigning fields Label { origin: Some(Origin([[R_U32_ORIGIN:[0-9]+]])){{.*}}}#&'a u32{{.*}}= Label { origin: Some(Origin({{.*}})){{.*}}&u32
    // CHECK-DAG: Aggregate literal label: Label{{.*}}origin_params: [('a, Origin([[R_U32_ORIGIN]])), ('h0, Origin({{.*}})]{{.*}}#S[]
    let x = 0;
    let ref_x = &x;
    let i = S {
        y: x,
        px: std::ptr::addr_of!(x),
        rx: ref_x,
        qx: ref_x,
    };
}

// CHECK-LABEL: final labeling for "tuple"
pub unsafe fn tuple() {
    let mut x = 2;
    let y = 1;

    // CHECK-DAG: ([[@LINE+1]]: px): &mut i32
    let px = std::ptr::addr_of_mut!(x);
    // CHECK-DAG: ([[@LINE+1]]: py): &i32
    let py = std::ptr::addr_of!(y);

    // CHECK-DAG: ([[@LINE+1]]: mut tup): (&mut i32, &i32)
    let mut tup = (px, py);
    *tup.0 = 3;
}

// CHECK-LABEL: final labeling for "aggregate1_array"
// CHECK-DAG: ([[@LINE+1]]: p): &std::cell::Cell<i32>
pub unsafe fn aggregate1_array(p: *mut i32) {
    // CHECK-DAG: ([[@LINE+1]]: arr): [&std::cell::Cell<i32>; 3]
    let arr = [p, p, p];
    *arr[0] = 1;
}

// CHECK-LABEL: final labeling for "aggregate1_array1"
// CHECK-DAG: ([[@LINE+1]]: p): &mut i32
pub unsafe fn aggregate1_array1(p: *mut i32) {
    // CHECK-DAG: ([[@LINE+1]]: arr): [&mut i32; 1]
    let arr = [p];
    *arr[0] = 1;
}

pub unsafe fn repeat() {
    let x = 22;
    let _buf: [u32; 22] = [x; 22];
}
