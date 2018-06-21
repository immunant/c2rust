// RUN: %clang_xcheck -O2 -o %t %s %xcheck_runtime %fakechecks
// RUN: %t 2>&1 | FileCheck %s

#include <stdio.h>
#include <stdint.h>

#include <cross_checks.h>

struct Foo {
    int a CROSS_CHECK("{ custom: ab_hash(a, b) }");
    int b;
};

uint64_t ab_hash(int a, int b) DISABLE_XCHECKS(true) {
    return a * 6631 + b;
}

int foo(struct Foo x CROSS_CHECK("default")) {
    return x.a + x.b;
}

int main() {
    struct Foo x = { 1000, 1337 };
    foo(x);
    return 0;
}
// CHECK: XCHECK(1):2090499946/0x7c9a7f6a
// CHECK: XCHECK(1):193491849/0x0b887389
// CHECK: XCHECK(3):4704780209814121655/0x414abcac80422cb7
// CHECK: XCHECK(2):193491849/0x0b887389
// CHECK: XCHECK(4):8680820740569198935/0x7878787878787157
// CHECK: XCHECK(2):2090499946/0x7c9a7f6a
// CHECK: XCHECK(4):8680820740569200758/0x7878787878787876
