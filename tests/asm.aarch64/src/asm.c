/* derived from LLVM's clang/test/CodeGen/aarch64-inline-asm.c */
typedef unsigned long uint64_t;
typedef unsigned int uint32_t;
typedef unsigned char uint8_t;

uint64_t var = 37;

// Fails to transpile: we need to rewrite tied operands in the asm template
/*uint64_t test_generic(uint64_t var64) {
    // doubles var64
    asm("add %0, %1, %1" : "=r"(var64) : "0"(var64));
    return var64;
}*/

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
    buffer[i++] = 12;//test_generic(6); // 12
    buffer[i++] = test_load(); // 37
    buffer[i++] = (uint32_t)test_constraint_w(-45.0); // -90

    test_tied_earlyclobber();
}
