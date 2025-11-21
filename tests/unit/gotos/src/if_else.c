int if_no_else(int x) {
    if (x > 0) {
        x += 1;
    }
    return x;
}

int if_else(int x) {
    if (x > 0) {
        x += 1;
    } else {
        x -= 1;
    }
    return x;
}

int nested_if_else(int x) {
    if (x > 0) {
        if (x < 10) {
            x += 1;
        } else {
            x += 2;
        }
    } else {
        x -= 1;
    }
    return x;
}

int nested_if_no_else(int x) {
    if (x > 0) {
        if (x < 10) {
            x += 1;
        }
        x += 2;
    } else {
        x -= 1;
    }
    return x;
}
