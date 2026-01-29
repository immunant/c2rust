// RUN: %clang_xcheck -O2 -o %t %s %xcheck_runtime %fakechecks
// RUN: %t 2>&1 | FileCheck %s

#include <stdio.h>
#include <string.h>

#include <cross_checks.h>

struct Foo {
    unsigned a: 7;
    unsigned b: 11;
};

int foo(struct Foo x DEFAULT_XCHECK) {
    return x.a;
}

struct Bar {
    unsigned int a;
    unsigned int b;
};

int bar(struct Bar x DEFAULT_XCHECK) {
    return x.a;
}

int main() {
    struct Foo x = { 42, 1337 };
    foo(x);

    unsigned int y1 = 0x55AA55AA;
    memcpy(&x, &y1, sizeof(y1));
    foo(x);

    // These should be the same as the above
    struct Bar by1 = { y1 & 0x7F, (y1 >> 7) & 0x7FF };
    bar(by1);

    unsigned int y2 = 0xAA55AA55;
    memcpy(&x, &y2, sizeof(x));
    foo(x);

    // These should be the same as the above
    struct Bar by2 = { y2 & 0x7F, (y2 >> 7) & 0x7FF };
    bar(by2);

    unsigned int y3 = 0x12345678;
    memcpy(&x, &y3, sizeof(x));
    foo(x);

    // These should be the same as the above
    struct Bar by3 = { y3 & 0x7F, (y3 >> 7) & 0x7FF };
    bar(by3);
    return 0;
}
// main()
// CHECK: XCHECK(Ent):2090499946/0x7c9a7f6a
//
// foo(x)
// CHECK: XCHECK(Ent):193491849/0x0b887389
// CHECK: XCHECK(Arg):2659199064397984410/0x24e75f75c47e329a
// CHECK: XCHECK(Exi):193491849/0x0b887389
// CHECK: XCHECK(Ret):8680820740569200732/0x787878787878785c
//
// foo(y1)
// CHECK: XCHECK(Ent):193491849/0x0b887389
// CHECK: XCHECK(Arg):2659199302229930284/0x24e75fad2461b12c
// CHECK: XCHECK(Exi):193491849/0x0b887389
// CHECK: XCHECK(Ret):8680820740569200732/0x787878787878785c
//
// bar(by1)
// CHECK: XCHECK(Ent):193487034/0x0b8860ba
// CHECK: XCHECK(Arg):2659199302229930284/0x24e75fad2461b12c
// CHECK: XCHECK(Exi):193487034/0x0b8860ba
// CHECK: XCHECK(Ret):8680820740569200732/0x787878787878785c
//
// foo(y2)
// CHECK: XCHECK(Ent):193491849/0x0b887389
// CHECK: XCHECK(Arg):14116302329485949165/0xc3e72e2d630778ed
// CHECK: XCHECK(Exi):193491849/0x0b887389
// CHECK: XCHECK(Ret):8680820740569200675/0x7878787878787823
//
// bar(by2)
// CHECK: XCHECK(Ent):193487034/0x0b8860ba
// CHECK: XCHECK(Arg):14116302329485949165/0xc3e72e2d630778ed
// CHECK: XCHECK(Exi):193487034/0x0b8860ba
// CHECK: XCHECK(Ret):8680820740569200675/0x7878787878787823
//
// foo(y3)
// CHECK: XCHECK(Ent):193491849/0x0b887389
// CHECK: XCHECK(Arg):13179962360378520869/0xb6e8a1efb3617525
// CHECK: XCHECK(Exi):193491849/0x0b887389
// CHECK: XCHECK(Ret):8680820740569200654/0x787878787878780e
//
// bar(by3)
// CHECK: XCHECK(Ent):193487034/0x0b8860ba
// CHECK: XCHECK(Arg):13179962360378520869/0xb6e8a1efb3617525
// CHECK: XCHECK(Exi):193487034/0x0b8860ba
// CHECK: XCHECK(Ret):8680820740569200654/0x787878787878780e
//
// main()
// CHECK: XCHECK(Exi):2090499946/0x7c9a7f6a
// CHECK: XCHECK(Ret):8680820740569200758/0x7878787878787876
