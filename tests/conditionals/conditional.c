void entry(const unsigned sz, int buf[const]) {
        int *x = &buf[0], *y = &buf[1];
        *(0 ? &y : &x) = 10;

        buf[2] = 1 ? 2 : 3;
        buf[3] = 0 ? 2 : 3;
}
