#include <stdint.h>

void test_builtin_expected_type(double d, int y) {
    int32_t ffs = __builtin_ffs(y);
    int32_t clz = __builtin_clz((unsigned)y);
    int32_t isnan = __builtin_isnan(d);
    int32_t isinf_sign = __builtin_isinf_sign(d);
    int32_t constant_p = __builtin_constant_p(y);
    int32_t cond = y ? __builtin_ffs(y) : 0;
}

void test_builtin_object_size(void) {
    int x = 0;
    unsigned long n = __builtin_object_size(&x, 1);
    unsigned long n2 = (unsigned long)__builtin_object_size(&x, 1);
}
