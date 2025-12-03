// This should translate to straightforward one-armed if's
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
