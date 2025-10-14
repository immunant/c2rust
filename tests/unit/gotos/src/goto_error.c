// Tests the common `goto error` pattern with a single error label.
void goto_error() {
    int COND = 0;

    // A
    if (COND) {
        // B
        if (COND) {
            // C
            goto error;
        }
        // D
    } else {
        // E
        if (COND) {
            // F
            goto error;
        }
        // G
    }
    // H
    return;

error:
    COND;
    // X
}

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
