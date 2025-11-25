// Tests the common `goto error` pattern with a single error label.
int goto_error_a(int x) {
    if (x >= 10) {
        if (x == 20) {
            x += 1;
            goto error;
        }
        x += 2;
    } else {
        if (x == 0) {
            x -= 1;
            goto error;
        }
        x -= 2;
    }
    return x + 3;

error:
    return x - 3;
}

// Similar behavior to `goto_error_a`, but at one point this one failed when the
// one above passed, so we want to keep both as a regression test.
int goto_error_b(int x) {
    if (x >= 10) {
        x += 2;
        if (x == 20) {
            x += 1;
            goto error;
        }
    } else {
        x -= 2;
        if (x == 0) {
            x -= 1;
            goto error;
        }
    }
    return x + 3;

error:
    return x - 3;
}

// The same control flow as `goto_error_b`, but using only gotos. This verifies
// that we can still reconstruct CFGs even when there is no structured control
// flow in the original C.
int goto_error_but_its_all_gotos(int x) {
    if (x >= 10) {
        goto B;
    } else {
        goto C;
    }

B:
    x += 2;
    if (x == 20) {
        x += 1;
        goto error;
    } else {
        goto D;
    }

C:
    x -= 2;
    if (x == 0) {
        x -= 1;
        goto error;
    } else {
        goto D;
    }

D:
    return x + 3;

error:
    return x - 3;
}

// `goto_error` with multiple error cases that then flow into a common
// termination case.
int goto_errors(int x) {
    if (x >= 10) {
        x += 1;
        if (x == 20) {
            x += 2;
            goto error_a;
        }
    } else {
        x -= 1;
        if (x == 0) {
            x -= 2;
            goto error_b;
        }
    }
    return x + 4;

error_a:
    x += 3;
    goto error_common;
error_b:
    x -= 3;
error_common:
    return x - 4;
}

// This has the same control-flow as `goto_errors`, but reorganized to have a
// single `goto` jumping to the success case.
int goto_success(int x) {
    if (x >= 10) {
        x += 1;
        if (x != 20) {
            goto success;
        }
        x += 2;
    } else {
        x -= 1;
        if (x != 0) {
            goto success;
        }
        x -= 2;
    }
    return x - 4;

success:
    return x + 4;
}

int goto_error_over_loop(int x)
{
    if (x > 10) {
        x++;
        if (x == 15)
            goto bad;
        x++;
    }

    for (int j = 0; j < 4; j++) {
        x++;
        if (x == 100)
            goto bad;
        x++;
    }

    return x;

bad:
    return -x;
}

int multi_goto_error_over_loop(int x)
{
    if (x > 10) {
        x++;
        if (x == 15)
            goto bad;
        x++;
    }

    if (x > 10) {
        x++;
        if (x == 15)
            goto also_bad;
        x++;
    }

    for (int j = 0; j < 4; j++) {
        x++;
        if (x == 100)
            goto bad;
        x++;
        if (x == 100)
            goto also_bad;
    }

    return x;

bad:
    return -x;

also_bad:
    return -2 * x;
}
