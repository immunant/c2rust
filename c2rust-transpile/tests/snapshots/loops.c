// These tests verify that we pull the right nodes into a loop when there are
// multiple paths out of the loop. When there are multiple paths out of a loop
// it's valid to leave all of those branches outside of the loop, but doing so
// means that we have to wrap the loop in extra labeled blocks so that we can
// jump to the appropriate code path when exiting the loop. We can avoid that by
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
            goto out;

        default:
            x += 3;
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
        break;

    default:
        x += 3;
        break;
    }

    x += 4;
    return x;
}
