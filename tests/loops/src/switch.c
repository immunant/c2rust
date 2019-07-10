// This code does not translate properly without the relooper
int switch_val(int val) {
    switch (val) {
        case 1:
            return 2;
        case 2:
            return 4;
        default:
            return val + 1;
    }
}
