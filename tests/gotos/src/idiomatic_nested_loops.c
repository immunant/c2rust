int simple_loop(int x) {
    while (x) {
        x--;
    }

    return x;
}

int nested_loop(int x) {
    while (x < 5) {
        while (x < 5) {
            x++;
        }
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

// This should translate to a straightforward `break` with a loop label 
int break_multiple(int x) {
  /* comment1 */

    while (x < 5) {
        /* comment2 */
        while (x < 5) {
            /* comment3 */
            while (x < 5) {
                /* comment4 */
                if (x < 2)
                    /* comment5 */
                    goto break_outer;
                /* comment6 */
                x++;
                /* comment7 */
            }
            /* comment8 */
        }
        /* comment9 */
    }
    /* comment10 */

break_outer:
    /* comment11 */
    x += 4;

    /* comment12 */
    return x;
    /* comment13 */
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
