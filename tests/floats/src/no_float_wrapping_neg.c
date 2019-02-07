// Previously this was generating 1.0f64.wrapping_neg() which does not exist
double no_wrapping_neg(void) {
    double a = -1.0;

    return a;
}

// Previously float types were getting an integer constant 1 for post/pre increment/decrement
float float_inc_dec(void) {
    float a = 1.1, b = 2.2;

    a++;
    b--;
    ++a;
    --b;

    a = ++b;
    a = b++;
    b = --a;
    b = a--;

    return a;
}

double double_inc_dec(void) {
    double a = 1.1, b = 2.2;

    a++;
    b--;
    ++a;
    --b;

    a = ++b;
    a = b++;
    b = --a;
    b = a--;

    return a;
}
