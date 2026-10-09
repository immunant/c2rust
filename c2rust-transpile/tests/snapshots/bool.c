#include <stdbool.h>

#define MACRO_INT0 0
#define MACRO_IDENT0 false

#define MACRO_INT1 1
#define MACRO_IDENT1 true

void test_bool() {
    bool int0 = 0;    
    bool ident0 = false;
    bool macro_int0 = MACRO_INT0;
    bool macro_ident0 = MACRO_IDENT0;

    bool int1 = 1;
    bool ident1 = true;
    bool macro_int1 = MACRO_INT1;
    bool macro_ident1 = MACRO_IDENT1;

    0 ? 2 : 3;
    false ? 2 : 3;
    MACRO_INT0 ? 2 : 3;
    MACRO_IDENT0 ? 2 : 3;

    1 ? 2 : 3;
    true ? 2 : 3;
    MACRO_INT1 ? 2 : 3;
    MACRO_IDENT1 ? 2 : 3;

    // Tests https://github.com/immunant/c2rust/issues/340
    int0 |= int1;

    // Casts to/from other numeric types
    // Regression test for https://github.com/immunant/c2rust/pull/2005#discussion_r4068559462
    int1 += 1U;
    int1 *= 2.0;
    double d = int1;
}

void test_bool_operator(void) {
    int comp_int = 1 < 0;
    long comp_long = 1 < 0;

    int logic_int = 0 || 0;
    long logic_long = 0 || 0;

    void *ptr = 0;
    int null_ptr_int = ptr == 0;
    long null_ptr_long = ptr == 0;

    int not_int = !0;
    long not_long = !0;
}
