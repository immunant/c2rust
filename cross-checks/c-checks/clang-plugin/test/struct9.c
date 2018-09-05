// RUN: %clang_xcheck -O2 -o %t %s %xcheck_runtime %fakechecks
// RUN: %t 2>&1 | FileCheck %s

#include <stdio.h>
#include <stdint.h>

#include <cross_checks.h>

// xmlErrorLevel from libxml2
typedef enum {
    XML_ERR_NONE = 0,
    XML_ERR_WARNING = 1,
    XML_ERR_ERROR = 2,
    XML_ERR_FATAL = 3,
} xmlErrorLevel;

// A copy of xmlError from libxml2
struct xmlError {
    int domain;
    int code;
    char *message;
    xmlErrorLevel level;
    char *file;
    int line;
    char *str1;
    char *str2;
    char *str3;
    int int1;
    int int2;
    void *ctxt;
    void *node;
};

struct xmlError foo(struct xmlError *x DEFAULT_XCHECK) {
    return *x;
}

int main() {
    struct xmlError x = { 5, 68, "h", XML_ERR_ERROR, "t", 975, NULL, NULL, NULL, 0, 2, &x, NULL };
    foo(&x);
    return 0;
}
// CHECK: XCHECK(Ent):2090499946/0x7c9a7f6a
// CHECK: XCHECK(Ent):193491849/0x0b887389
// CHECK: XCHECK(Arg):9325795713530749616/0x816be1ca90dc62b0
// CHECK: XCHECK(Exi):193491849/0x0b887389
// CHECK: XCHECK(Ret):9325795713530749616/0x816be1ca90dc62b0
// CHECK: XCHECK(Exi):2090499946/0x7c9a7f6a
// CHECK: XCHECK(Ret):8680820740569200758/0x7878787878787876
