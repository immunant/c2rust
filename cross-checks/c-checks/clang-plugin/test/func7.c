// RUN: %clang_xcheck -O2 -o %t %s %xcheck_runtime %fakechecks
// RUN: %t 2>&1 | FileCheck %s

#include <stdio.h>
#include <stdint.h>

#include <cross_checks.h>

uint64_t xcheck_foo(void) DISABLE_XCHECKS(true) {
    return 0x1234;
}

int foo() CROSS_CHECK("{ entry: { custom: xcheck_foo }, exit: { custom: xcheck_foo } }") {
    return 1;
}

int main() {
    foo();
    return 0;
}
// CHECK: XCHECK(1):2090499946/0x7c9a7f6a
// CHECK: XCHECK(1):4660/0x00001234
// CHECK: XCHECK(2):4660/0x00001234
// CHECK: XCHECK(4):8680820740569200759/0x7878787878787877
// CHECK: XCHECK(2):2090499946/0x7c9a7f6a
// CHECK: XCHECK(4):8680820740569200758/0x7878787878787876
