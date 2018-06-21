// RUN: %clang_xcheck %fakechecks -O2 -o %t %s %xcheck_runtime
// RUN: %t 2>&1 | FileCheck %s

#include <stdio.h>

#include <cross_checks.h>

int foo() CROSS_CHECK("{ entry: { djb2: main }, exit: disabled }") {
    return 1;
}

int main() {
    foo();
    return 0;
}
// CHECK: XCHECK(1):2090499946/0x7c9a7f6a
// CHECK: XCHECK(1):2090499946/0x7c9a7f6a
// CHECK: XCHECK(4):8680820740569200759/0x7878787878787877
// CHECK: XCHECK(2):2090499946/0x7c9a7f6a
// CHECK: XCHECK(4):8680820740569200758/0x7878787878787876
