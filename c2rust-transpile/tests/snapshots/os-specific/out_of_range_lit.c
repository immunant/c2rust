#include <stdint.h>

void f() {
    int32_t a = 0x80000000U;
    int32_t b = 0x80000000;
    int32_t c = 0x8000000000000000;

    int32_t d = (unsigned int)0x80000000U; // Test with explicit cast.
}
