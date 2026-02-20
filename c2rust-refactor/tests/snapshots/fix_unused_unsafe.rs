fn main() {
    // Has unsafe operations, shouldn't be modified.
    unsafe {
        let val = 123;
        let ptr = &val as *const i32;
        let _safe_val = *ptr;
    }

    unsafe {
        // Block with no comment and no destructors, block should be removed but
        // contents kept.
        let x = 42;
    }

    unsafe {}

    // Has a comment, shouldn't be removed.
    unsafe {}

    unsafe {
        // Contains a destructor, keep the block to avoid changing drop order.
        let _string = String::from("string");
    }

    // Block expression should stay.
    let _ = unsafe { 5 + 5 };
}
