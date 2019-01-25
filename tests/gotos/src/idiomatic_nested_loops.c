//! enable_relooper

// This shoud translate to a straightforward `break` with a loop label 
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
    x += 4;

    return x;
}
