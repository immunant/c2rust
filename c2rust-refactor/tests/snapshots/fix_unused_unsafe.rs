fn main() {
    // Has unsafe operations, shouldn't be modified.
    unsafe {
        let val = 123;
        let ptr = &val as *const i32;
        let _safe_val = *ptr;
    }

    unsafe {
        // Block expr with no comment and no destructors, block should be
        // removed but contents kept.
        let x = 42;
    }

    unsafe {}

    // Has a comment, shouldn't be removed.
    unsafe {}

    unsafe {
        // Contains a destructor, keep the block to avoid changing drop order.
        let _string = String::from("string");
    }

    // Block expr with no statements, we can remove the block.
    let _ = unsafe { 5 + 5 };

    // Block expr with statements, keep the block.
    let _ = unsafe {
        let _ = 5;
        5 + 5
    };

    // TODO: Uh oh, if there's a comment on the tail expr we eat it.
    // let _ = unsafe {
    //     // Comment on the expr.
    //     5 + 5
    // };
    let _ = {};

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

    unsafe {
        // Trailing expr in an unsafe block used as a statement, make sure we
        // don't just remove the block since that would turn the statement into
        // an expression.
        // TODO: Why is there extra whitespace between lines of the comment?
        1 + 2
    };
}
