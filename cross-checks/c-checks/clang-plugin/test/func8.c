// RUN: %clang_xcheck -O2 -o %t %s %xcheck_runtime %fakechecks
// RUN: %t 2>&1 | FileCheck %s

#include <stdio.h>
#include <stdint.h>

#include <cross_checks.h>

uint64_t mul_17(int x) DISABLE_XCHECKS(true) {
    return x * 17;
}

uint64_t deref_mul_31(int *x) DISABLE_XCHECKS(true) {
    return x ? (*x * 31) : 0xDEADBEEF;
}

int foo(int *x CUSTOM_XCHECK("mul_17(*x)"),
        int *y CUSTOM_XCHECK("deref_mul_31(y)"),
        int  z CUSTOM_XCHECK("deref_mul_31(&z)")) {
    return *x + *y + z;
}

int main() {
    int x = 256;
    foo(&x, &x, 3 * x);
    return 0;
}
// CHECK: XCHECK(1):2090499946/0x7c9a7f6a
// CHECK: XCHECK(1):193491849/0x0b887389
// CHECK: XCHECK(3):4352/0x00001100
// CHECK: XCHECK(3):7936/0x00001f00
// CHECK: XCHECK(3):23808/0x00005d00
// CHECK: XCHECK(2):193491849/0x0b887389
// CHECK: XCHECK(4):8680820740569202038/0x7878787878787d76
// CHECK: XCHECK(2):2090499946/0x7c9a7f6a
// CHECK: XCHECK(4):8680820740569200758/0x7878787878787876
