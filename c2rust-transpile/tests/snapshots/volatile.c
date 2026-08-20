struct S { int * volatile p; } volatile_global_struct;

void test_volatile(void) {
    volatile int vi = 0;
    int i = vi;
    vi = i;

    volatile int *pvi = &vi;
    i = *pvi;
    *pvi = i;

    // https://github.com/immunant/c2rust/issues/1237
    --(volatile_global_struct.p);

    // Unused reads, should be included as side effects.
    vi;
    -vi;
    vi + 1;
    *pvi;
    volatile_global_struct.p;
    volatile_global_struct.p + 1;
    *volatile_global_struct.p;
}
