// Basic loop structures.

int while_loop(int x) {
    while (x) {
        x += 1;
    }
    return x;
}

int for_loop(int x) {
    for (int i = 0; i < 10; i++) {
        x += 1;
    }
    return x;
}

void infinite_loop(int x) {
    for (;;) {
        x += 1;
    }
}

int goto_loop(int x) {
loop:
    x += 1;
    if (x) {
        goto loop;
    }
    return x;
}

int do_while_loop(int x) {
    do {
        x += 1;
    } while (x);
    return x;
}

// `continue` in `for` and `do` loops.
//
// `for` and `do` loops are fun because they require doing something at the
// *end* of every loop, which we don't have a direct analog for in Rust. When
// there's a `continue` in a `for` loop, we can't just `continue` in the
// translated Rust because that would skip the logic that's supposed to happen
// at the end of the loop. To handle this we instead generate a block in the
// loop body and use a labeled `break` to jump to the logic at the end of the
// loop.

int for_loop_single_continue(int x) {
    for (int i = 0; i < 10; i++) {
        if (x) {
            if (x)
                continue;
        }
        x += 1;
    }
    return x;
}

int for_loop_multi_continue(int x) {
    for (int i = 0; i < 10; i++) {
        if (x)
            if (x)
                continue;
        x += 1;
        if (x)
            if (x)
                continue;
        x += 2;
    }
    return x;
}

int do_loop_single_continue(int x) {
    do {
        if (x) {
            if (x)
                continue;
        }
        x += 1;
    } while (x);
    return x;
}

int do_loop_multi_continue(int x) {
    do {
        if (x)
            if (x)
                continue;
        x += 1;
        if (x)
            if (x)
                continue;
        x += 2;
    } while (x);
    return x;
}

// These tests verify that we pull the right nodes into a loop when there are
// multiple paths out of the loop. When there are multiple paths out of a loop
// it's valid to leave all of those branches outside of the loop, but doing so
// means that we have to wrap the loop in extra labeled blocks so that we can
// jump to the appropriate code path when exiting the loop. We avoid that by
// pulling in any nodes that can be cleanly inlined into a branch within the
// loop.

int explicit_loop_multi_exit(int x) {
    for (;;) {
        switch (x) {
        case 1:
            x += 1;
            continue;

        case 2:
            x += 2;
            if (x)
                x += 3;
            else
                x += 4;
            x += 5;
            goto out;

        default:
            x += 6;
            goto out;
        }
    }

out:
    x += 4;
    return x;
}

int implicit_loop_multi_exit(int x) {
redo:
    switch (x) {
    case 1:
        x += 1;
        goto redo;

    case 2:
        x += 2;
        if (x)
            x += 3;
        else
            x += 4;
        x += 5;
        break;

    default:
        x += 6;
        break;
    }

    x += 4;
    return x;
}
