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

struct Foo {
    a: i32,
    b: i32,
}

// CHECK-LABEL: fn shared_ref_with_struct() {
fn shared_ref_with_struct() {
    let x = Foo { a: 1, b: 2 };
    // CHECK-DAG: let y = &(x.a);
    let y = std::ptr::addr_of!(x.a);
}

// CHECK-LABEL: fn cast_array_to_ptr_explicit(s: &[u8; 1]) {
pub fn cast_array_to_ptr_explicit(s: &[u8; 1]) {
    // CHECK-DAG: &*(std::ptr::addr_of!(*s)) as *const u8
    std::ptr::addr_of!(*s) as *const u8;
}
