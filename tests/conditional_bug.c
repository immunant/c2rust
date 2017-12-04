// expect failure
void entry(const unsigned sz, int buf[const]) {
        int x = 0, y = 0;
        *(0 ? &y : &x) = 10;
}
