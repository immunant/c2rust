// RUN: %clang_xcheck -O2 -o %t %s %xcheck_runtime %fakechecks
// RUN: %t 2>&1 | FileCheck %s

#include <stdio.h>

#include <cross_checks.h>

struct Foo {
    // Fixed value cross-checks for both fields,
    // checks should be identical
    int a FIXED_XCHECK("0x1234");
    int b FIXED_XCHECK("0xabcd");
};

int foo(struct Foo x DEFAULT_XCHECK) {
    return x.a + x.b;
}

int main() {
    struct Foo x = { 1000, 1337 };
    foo(x);

    struct Foo y = { 2000, 2337 };
    foo(y);
    return 0;
}
// CHECK: XCHECK(Ent):2090499946/0x7c9a7f6a
// CHECK: XCHECK(Ent):193491849/0x0b887389
// CHECK: XCHECK(Arg):12587252335133184481/0xaeaee74f05d1d1e1
// CHECK: XCHECK(Exi):193491849/0x0b887389
// CHECK: XCHECK(Ret):8680820740569198935/0x7878787878787157
// CHECK: XCHECK(Ent):193491849/0x0b887389
// CHECK: XCHECK(Arg):12587252335133184481/0xaeaee74f05d1d1e1
// CHECK: XCHECK(Exi):193491849/0x0b887389
// CHECK: XCHECK(Ret):8680820740569196679/0x7878787878786887
// CHECK: XCHECK(Exi):2090499946/0x7c9a7f6a
// CHECK: XCHECK(Ret):8680820740569200758/0x7878787878787876
