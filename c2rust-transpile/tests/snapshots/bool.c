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

    // Doesn't compile currently.
    // See https://github.com/immunant/c2rust/issues/340.
    int0 |= int1;
}
