// RUN: %clang_xcheck -O2 -o %t %s %xcheck_runtime %fakechecks
// RUN: %t 2>&1 | FileCheck %s

#include <stdio.h>
#include <stdint.h>

#include <cross_checks.h>

// Structure with custom hash function
struct CUSTOM_HASH_XCHECK("my_Foo_hash") Foo {
    int a;
    int b;
};

uint64_t my_Foo_hash(struct Foo *x) DISABLE_XCHECKS(true) {
    return 0x12345678ULL;
}

int foo(struct Foo x DEFAULT_XCHECK) {
    return x.a + x.b;
}

int main() {
    struct Foo x = { 1000, 1337 };
    foo(x);
    return 0;
}
// CHECK: XCHECK(Ent):2090499946/0x7c9a7f6a
// CHECK: XCHECK(Ent):193491849/0x0b887389
// CHECK: XCHECK(Arg):305419896/0x12345678
// CHECK: XCHECK(Exi):193491849/0x0b887389
// CHECK: XCHECK(Ret):8680820740569198935/0x7878787878787157
// CHECK: XCHECK(Exi):2090499946/0x7c9a7f6a
// CHECK: XCHECK(Ret):8680820740569200758/0x7878787878787876
