int simple_loop(int x) {
    while (x) {
        x--;
    }

    return x;
}

int infinite_loop_with_break(int x) {
    // Also we have a different comment here, just to be difficult.
    for (;;) {
        // Imagine we have a comment here. If we smoosh this into a `while x` loop, where should the comment go?
        if (!x)
            break;
        x--;
    }

    return x;
}

int nested_for(int x) {
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            x++;
        }
    }
    return x;
}

int nested_loop(int x) {
    while (x < 5) {
        while (x < 5) {
            x++;
        }
        x++;
    }

    return x;
}

int trivially_nested_loop(int x) {
    while (x < 5) {
        while (x < 5) {
            if (x < 2)
                goto break_outer;
            x++;
        }
    }

break_outer:
    return x;
}

// The same cfg as `trivially_nested_loop`, but expressed in a weirder way. TBD
// if we want to generate the same code for both of these functions.
int weird_nested_loops(int x) {
outer:
    while (x < 5) {
        for (;;) {
            if (!(x < 5))
                goto outer; // Explicitly jump back to the top of the outer loop instead of breaking forward out of the inner loop.
            if (x < 2)
                goto break_outer;
            x++;
        }
    }

break_outer:
    return x;
}

// This should translate to a straightforward `break` with a loop label 
int break_multiple(int x) {
    while (x < 5) {
        while (x < 5) {
            while (x < 5) {
                if (x < 2)
                    goto break_outer;
                x++;
            }
        }
    }

break_outer:
    return x + 4;
}

int do_while_with_breaks(int x) {
    do {
        if (x) {
            if (x) {
                x++;
                break;
            }
        }

        if (x)
            break;
    } while(1);

    return x;
}

int my_printf_simplified(int x) {
    if (x) {
        switch (x) {
            case 0:
                x++;
                break;
            case 1:
                x++;
                break;
            case 2:
                x++;
                break;
        }
    }

    return x;
}
