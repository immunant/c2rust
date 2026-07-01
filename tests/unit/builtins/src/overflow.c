#include <limits.h>
#include <stddef.h>

// Operands of distinct same-width types (size_t and unsigned long translate
// to the distinct Rust types usize and c_ulong even on LP64).
void overflow_builtins(const unsigned buffer_size, int buffer[const])
{
    int i = 0;
    size_t sz = 1000;
    unsigned long ul = 7;
    size_t out_sz = 0;
    unsigned long out_ul = 0;

    // One common type, so these map to Rust's native `overflowing_*` methods.
    buffer[i++] = __builtin_add_overflow(sz, out_sz, &out_sz);
    buffer[i++] = (int)out_sz;
    buffer[i++] = __builtin_mul_overflow(ul, ul, &out_ul);
    buffer[i++] = (int)out_ul;

    buffer[i++] = __builtin_mul_overflow(sz, ul, &out_sz);
    buffer[i++] = (int)out_sz;
    buffer[i++] = __builtin_add_overflow(sz, ul, &out_sz);
    buffer[i++] = (int)out_sz;
    buffer[i++] = __builtin_sub_overflow(ul, sz, &out_ul);
    buffer[i++] = (int)out_ul;

    // Actually overflowing.
    buffer[i++] = __builtin_mul_overflow((size_t)-1, (unsigned long)2, &out_sz);
    buffer[i++] = (int)out_sz;
    buffer[i++] = __builtin_mul_overflow((size_t)-1, (unsigned long)-1, &out_sz);
    buffer[i++] = (int)out_sz;
}

// The `s`/`u`-prefixed variants fix one common type, so they also map to
// Rust's native `overflowing_*` methods.
void overflow_builtins_prefixed(const unsigned buffer_size, int buffer[const])
{
    int i = 0;
    unsigned out_u = 0;
    unsigned long long out_ull = 0;
    int out_i = 0;
    long out_l = 0;

    buffer[i++] = __builtin_uadd_overflow(UINT_MAX, 1u, &out_u);
    buffer[i++] = (int)out_u;
    buffer[i++] = __builtin_uaddll_overflow(ULLONG_MAX, 2ULL, &out_ull);
    buffer[i++] = (int)out_ull;
    buffer[i++] = __builtin_usub_overflow(0u, 1u, &out_u);
    buffer[i++] = (int)out_u;
    buffer[i++] = __builtin_sadd_overflow(INT_MAX, 1, &out_i);
    buffer[i++] = out_i;
    buffer[i++] = __builtin_smull_overflow(LONG_MAX, 2L, &out_l);
    buffer[i++] = (int)out_l;
    buffer[i++] = __builtin_smul_overflow(3, 5, &out_i);
    buffer[i++] = out_i;
}

// Same-type 128-bit uses also map to Rust's native `overflowing_*` methods
// (mixed-type uses involving 128-bit integers are rejected instead).
void overflow_builtins_uint128(const unsigned buffer_size, int buffer[const])
{
    int i = 0;
    __uint128_t max = ~(__uint128_t)0;
    __uint128_t one = 1;
    __uint128_t three = 3;
    __uint128_t four = 4;
    __uint128_t out = 0;

    buffer[i++] = __builtin_add_overflow(max, one, &out);
    buffer[i++] = (int)(out >> 64);
    buffer[i++] = (int)out;

    // (2^128 - 1) is divisible by 3, so this fits exactly.
    buffer[i++] = __builtin_mul_overflow(max / three, three, &out);
    buffer[i++] = (int)(out >> 64);
    buffer[i++] = (int)out;
    buffer[i++] = __builtin_mul_overflow(max / three, four, &out);
    buffer[i++] = (int)(out >> 64);
    buffer[i++] = (int)out;

    buffer[i++] = __builtin_sub_overflow(one, max, &out);
    buffer[i++] = (int)(out >> 64);
    buffer[i++] = (int)out;
}

// Result type narrower than the operands.
void overflow_builtins_narrow_result(const unsigned buffer_size, int buffer[const])
{
    int i = 0;
    long long ll1 = 5000000000LL;
    long long ll2 = 5000000000LL;
    int narrow_out = 0;
    buffer[i++] = __builtin_add_overflow(ll1, ll2, &narrow_out);
    buffer[i++] = narrow_out;

    long long ll3 = -5000000000LL;
    long long ll4 = 3LL;
    buffer[i++] = __builtin_sub_overflow(ll3, ll4, &narrow_out);
    buffer[i++] = narrow_out;

    long long ll5 = 5000000000LL;
    long long ll6 = 2LL;
    buffer[i++] = __builtin_mul_overflow(ll5, ll6, &narrow_out);
    buffer[i++] = narrow_out;
}

// Operands of mixed signedness and width.
void overflow_builtins_mixed(const unsigned buffer_size, int buffer[const])
{
    int i = 0;
    size_t out_sz = 0;
    long long out_ll = 0;

    // -1 + 0 is not representable in size_t.
    int negative = -1;
    size_t sz = 0;
    buffer[i++] = __builtin_add_overflow(negative, sz, &out_sz);
    buffer[i++] = (int)(out_sz >> 32);
    buffer[i++] = (int)out_sz;

    // 7 + 1000 is.
    int seven = 7;
    sz = 1000;
    buffer[i++] = __builtin_add_overflow(seven, sz, &out_sz);
    buffer[i++] = (int)out_sz;

    // Product of distinct operand types overflows even 128-bit precision.
    unsigned long long ull_max = ~0ULL;
    buffer[i++] = __builtin_mul_overflow(ull_max, (size_t)-1, &out_sz);
    buffer[i++] = (int)out_sz;

    // Unsigned operands, signed result.
    unsigned above_int_max = 3000000000u;
    unsigned long nine = 9;
    buffer[i++] = __builtin_add_overflow(above_int_max, nine, &out_ll);
    buffer[i++] = (int)(out_ll >> 32);
    buffer[i++] = (int)out_ll;
}
