use std::ptr;

// CHECK-LABEL: final labeling for "alias1_good"
pub unsafe fn alias1_good() {
    // CHECK-DAG: ([[#@LINE+1]]: mut x): addr_of = READ | WRITE | UNIQUE,
    let mut x = 0;
    // CHECK-DAG: ([[#@LINE+1]]: p): {{.*}}type = UNIQUE#
    let p = ptr::addr_of_mut!(x);
    // CHECK-DAG: ([[#@LINE+1]]: q): {{.*}}type = READ | WRITE | UNIQUE#
    let q = ptr::addr_of_mut!(x);
    *q = 1;
}

// CHECK-LABEL: final labeling for "alias1_bad"
pub unsafe fn alias1_bad() {
    // CHECK-DAG: ([[#@LINE+2]]: mut x): addr_of = READ | WRITE,
    // CHECK-DAG: ([[#@LINE+1]]: mut x): addr_of flags = CELL,
    let mut x = 0;
    // CHECK-DAG: ([[#@LINE+1]]: p): {{.*}}type = READ | WRITE#
    let p = ptr::addr_of_mut!(x);
    // CHECK-DAG: ([[#@LINE+2]]: q): {{.*}}type = (empty)#
    // CHECK-DAG: ([[#@LINE+1]]: q): {{.*}}type flags = CELL#
    let q = ptr::addr_of_mut!(x);
    *p = 1;
}


// The safe versions of these functions are useful for debugging Polonius fact generation, but
// aren't checked when running tests.
#[cfg(debug_polonius_facts)]
pub fn safe_alias1_good() {
    let mut x = 0;
    let p = &mut x;
    let q = &mut x;
    *q = 1;
}

#[cfg(debug_polonius_facts)]
pub fn safe_alias1_bad() {
    let mut x = 0;
    let p = &mut x;
    let q = &mut x;
    *p = 1;
}
