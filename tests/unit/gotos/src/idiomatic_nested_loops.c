int simple_loop(int x) {
    while (x) {
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
