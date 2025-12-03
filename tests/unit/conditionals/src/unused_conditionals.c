// Regression case for issue #147 where side-effects of unused binary expressions are swallowed up.

int inc(int *n) {
    *n++;
    return 1;
}

int unused_conditional1(void) {
    int i = 2;
    0 < inc(&i);
    return i;
}

int unused_conditional2(void) {
    int i = 2;
    0 || inc(&i);
    return i;
}

int unused_conditional3(void) {
    int i = 2;
    0 && inc(&i);
    return i;
}
