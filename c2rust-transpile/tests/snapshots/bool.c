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

#define STR "ab"
#define WSTR L"ab"

void test_ptr(int arg) {
    int not_wstr = !&L"ab";
    int conditional_wstr = &L"ab" ? 1 : 2;
    int and_wstr = &L"ab" && arg;
    int cast_wstr = (_Bool) &L"ab";

    int not_macro_str = !&STR;
    int not_macro_wstr = !&WSTR;
    int conditional_macro_str = &STR ? 1 : 2;
}
