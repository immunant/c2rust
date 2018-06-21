// RUN: %clang_xcheck -O2 -o %t %s %xcheck_runtime %fakechecks
// RUN: %t 2>&1 | FileCheck %s

#include <stdio.h>

#include <cross_checks.h>

int foo() DISABLE_XCHECKS(true) {
    return 1;
}

int main() {
    foo();
    return 0;
}
// CHECK: XCHECK(1):2090499946/0x7c9a7f6a
// CHECK-NOT: XCHECK(1):193491849/0x0b887389
// CHECK-NOT: XCHECK(2):193491849/0x0b887389
// CHECK: XCHECK(2):2090499946/0x7c9a7f6a
// CHECK: XCHECK(4):8680820740569200758/0x7878787878787876
