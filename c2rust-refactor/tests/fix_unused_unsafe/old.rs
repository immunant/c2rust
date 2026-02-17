fn main() {
    // Has unsafe operations, shouldn't be modified.
    unsafe {
        let val = 123;
        let ptr = &val as *const i32;
        let _safe_val = *ptr;
    }

    unsafe {
        let x = 42;
    }

    unsafe {}

    let _ = unsafe { 5 + 5 };
}
