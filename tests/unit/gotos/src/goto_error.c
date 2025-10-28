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

// Should have the same behavior as `goto_error_a`, but at one point this one
// failed when the one above passed, so we want to keep both as a regression
// test.
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

/*
// `goto_error` with multiple error cases that then flow into a common
// termination case.
void goto_errors() {
    int COND = 0;

    // A
    if (COND) {
        // B
        if (COND) {
            goto error_a;
        }
    } else {
        // C
        if (COND) {
            goto error_b;
        }
    }
    // D
    // E
    return;

error_a:
    // X
    goto error_common;
error_b:
    // Y
    COND;
error_common:
    // Z
    COND;
}

// This has the same control-flow as `goto_errors`, but reorganized to have a
// single `goto` jumping to the success case.
void goto_success() {
    int COND = 0;

    // A
    if (COND) {
        // B
        if (COND) {
            goto success;
        }
        // X
    } else {
        // C
        if (COND) {
            goto success;
        }
        // Y
    }
    // Z
    return;

success:
    // D
    // E
    COND;
}
*/
