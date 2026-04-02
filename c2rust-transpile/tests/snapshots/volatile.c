struct S { int * volatile p; } volatile_global_struct;

void test_volatile(void) {
    // https://github.com/immunant/c2rust/issues/1237
    --(volatile_global_struct.p);
}
