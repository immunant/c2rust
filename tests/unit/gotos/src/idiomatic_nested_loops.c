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
