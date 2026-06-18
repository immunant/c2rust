#include <stddef.h>

void overflow_builtins(const unsigned buffer_size, int buffer[const])
{
    int i = 0;
    size_t sz = 1000;
    unsigned long ul = 7;
    size_t out_sz = 0;
    unsigned long out_ul = 0;

    // Operands of distinct same-width types (size_t vs unsigned long).
    buffer[i++] = __builtin_mul_overflow(sz, ul, &out_sz);
    buffer[i++] = (int)out_sz;
    buffer[i++] = __builtin_add_overflow(sz, ul, &out_sz);
    buffer[i++] = (int)out_sz;
    buffer[i++] = __builtin_sub_overflow(ul, sz, &out_ul);
    buffer[i++] = (int)out_ul;

    // Actually overflowing.
    buffer[i++] = __builtin_mul_overflow((size_t)-1, (unsigned long)2, &out_sz);
    buffer[i++] = (int)out_sz;
}
