#include <stdbool.h>
#include <math.h>

void long_double_test(void) {
    long double one = 1.0l;
    long double zero;
    long double add = one + zero;
    long double huge = __builtin_huge_vall();

    int i = one;
    float f = one;
    long double cast_from_int = i;
    long double cast_from_float = f;

    bool is_inf = isinf(huge);
    if (one) {
        int dummy;
    }
}

void float128_test(void) {
    __float128 one = 1.0q;
    __float128 zero;
    __float128 add = one + zero;
    __float128 huge = __builtin_huge_vall();

    int i = one;
    float f = one;
    __float128 cast_from_int = i;
    __float128 cast_from_float = f;
    long double ld_from_float128 = one;

    bool is_inf = isinf(huge);
    if (one) {
        int dummy;
    }
}
