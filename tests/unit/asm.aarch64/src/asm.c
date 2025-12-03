//! skip_translation
/* derived from LLVM's clang/test/CodeGen/aarch64-inline-asm.c */
typedef unsigned long uint64_t;
typedef unsigned int uint32_t;
typedef unsigned char uint8_t;

uint64_t var = 37;

uint64_t test_generic(uint64_t var64)
{
    // doubles var64
    asm("add %0, %1, %1"
        : "=r"(var64)
        : "0"(var64));
    return var64;
}

uint64_t test_generic2(uint64_t var64)
{
    uint64_t tmp1 = 1;
    uint64_t tmp2 = 0;
    asm("add %0, %3, %3\n\t" // double var64
        "add %2, %5, %5\n\t" // double original var64, put in tmp2
        "add %2, %4, %2" // add tmp2 and tmp1, put in tmp2
        : "=r"(var64), "=r"(var64), "+r"(tmp2)
        : "0"(var64), "r"(tmp1), "1"(var64));

    // (var64 * 2) + (var64 * 2 + 1)
    return var64 + tmp2;
}

uint64_t test_generic3(void) {
    uint64_t out = 0;
    uint64_t one = 1;
    uint64_t two = 2;
    uint64_t three = 3;
    uint64_t tmp1, tmp2, tmp3;

    asm("add %0, %0, %4\n\t"
        "add %0, %0, %5\n\t"
        "add %0, %0, %6"
        : "=r"(out), "=r"(tmp1),"=r"(tmp2), "=r"(tmp3)
        : "1"(one), "2"(two), "r"(three));

    return out;
}

uint64_t test_load(void) {
    uint64_t var64;
    // load from var into var64
    asm("ldr %0, [%1]" : "=r"(var64) : "r"(&var));
    return var64;
}

double test_constraint_w(double d) {
    asm("fadd %d0, %d1, %d1" : "=w"(d) : "w"(d));
    return d;
}

uint32_t test_constraint_Q(void) {
    uint32_t val;
    asm("ldxr %0, %1" : "=r"(val) : "Q"(var));
    return val;
}

void test_tied_earlyclobber(void) {
  register int a asm("x1");
  asm("/* %x0 */" : "+&r"(a));
}

void entry(const unsigned int buffer_size, int buffer[const])
{
    int i = 0;

    buffer[i++] = test_constraint_Q(); // 37
    buffer[i++] = test_generic(6); // 12
    buffer[i++] = test_generic2(6); // 25
    buffer[i++] = test_generic3(); // 6
    buffer[i++] = test_load(); // 37
    buffer[i++] = (uint32_t)test_constraint_w(-45.0); // -90

    test_tied_earlyclobber();
}
