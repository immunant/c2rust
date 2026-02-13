fn main() {
    // Has unsafe operations, shouldn't be modified.
    unsafe {
        let val = 123;
        let ptr = &val as *const i32;
        let _safe_val = *ptr;
    }

    // Block with statements and no `Drop`, block should be removed completely
    // and replaced with statements.
    unsafe {
        let x = 42;
    }

    // Empty block with a comment; block should remain so the comment is
    // preserved. The following block has no comment and should be removed.
    unsafe {}

    unsafe {}

    let _ = unsafe { 5 + 5 };
}
