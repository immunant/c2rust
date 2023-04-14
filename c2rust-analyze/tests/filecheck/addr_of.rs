// CHECK-LABEL: fn shared_ref() {
fn shared_ref() {
    let x = 1;
    // CHECK-DAG: let y = &(x);
    let y = std::ptr::addr_of!(x);
}

// CHECK-LABEL: unsafe fn mut_ref() {
unsafe fn mut_ref() {
    let mut z = 2;
    // CHECK-DAG: let x = &mut (z);
    let x = std::ptr::addr_of_mut!(z);
    *x = *x;
}

// TODO: check rewrite to Cell type and operations
unsafe fn cell() {
    let mut x = 1;
    let mut y = std::ptr::addr_of_mut!(x);
    let mut z = std::ptr::addr_of_mut!(x);
    *z = 1;
    *y = 1;
}
