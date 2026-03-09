int simple_if_else(int x) {
    if (x) {
        x += 1;
    } else {
        x += 2;
    }
    return x;
}


int if_no_else(int x) {
    if (x > 0) {
        x += 1;
    }
    return x;
}

int multiple_ifs(int x) {
    if (x)
        x += 1;

    if (x)
        x += 2;

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

// Early returns can result in an ambiguous CFG where it's not clear if the `if`
// should have an `else` or not. This can result in us moving code into an
// `else` branch when the original was not structured that way.

int early_returns(int a) {
    if (a == 2) {
        return 2;
    }
    if (a == 3) {
        a += 1;
    }
    if (a == 4) {
        return 1;
    }
    return 0;
}

int nested_early_returns(int x) {
    if (x) {
        if (x) {
            x += 1;
            return x;
        }
        x += 2;
    } else {
        if (x) {
            x += 3;
            return x;
        }
        x += 4;
    }
    return x;
}
