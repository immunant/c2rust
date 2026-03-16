fn main() {
    // Unsafe block should stay even if there is a compile error later.
    unsafe {
        let val = 123;
        let ptr = &val as *const i32;
        let _safe_val = *ptr;
    }

    // Introduce a compile error after the unsafe block.
    undefined_name;
}
