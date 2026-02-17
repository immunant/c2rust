fn main() {
    // Has unsafe operations, shouldn't be modified.
    unsafe {
        let val = 123;
        let ptr = &val as *const i32;
        let _safe_val = *ptr;
    }

    {
        let x = 42;
    }

    {}

    let _ = { 5 + 5 };
}
