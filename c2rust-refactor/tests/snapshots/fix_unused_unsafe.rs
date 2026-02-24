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

    unsafe {
        // Contains a comment but no body. In theory we'd like to preserve this,
        // but it's hard to detect the comment since it doesn't have an AST node
        // it's attached to. For now we allow this to be deleted.
    }

    unsafe {
        unsafe {
            // Nested unsafe blocks. Not that hard to handle, but also not a
            // case that we need to cover since c2rust doesn't generate code
            // like this and later refactoring is unlikely to introduce it.
            let y = 99;
        }
    }
}
