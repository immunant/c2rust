void if_else_loops() {
    int COND = 0;

    // A
    if (COND) {
        do {
            // B
        } while (COND);
    } else {
        do {
            // C
        } while (COND);
    }
    // D
}

void goto_loops() {
    int COND = 0;

    // A
    if (COND) {
        goto B;
    } else {
        goto C;
    }

B:
    // B
    if (COND) {
        goto B;
    } else {
        goto D;
    }

C:
    // C
    if (COND) {
        goto C;
    } else {
        goto D;
    }

D:
    // D
}


void goto_into_loops() {
    int COND = 0;

    // A
    if (COND) {
        goto B;
    } else {
        goto C;
    }

    do {
B:
        // B
    } while (COND);
    goto D;

    do {
C:
        // C
    } while (COND);
    goto D;

D:
    // D
}

void goto_separate_loops() {
    int COND = 0;

    // A
    if (COND) {
        goto B;
    } else {
        goto C;
    }

B:
    do {
        // B
    } while (COND);
    goto D;

C:
    do {
        // C
    } while (COND);
    goto D;

D:
    // D
}
