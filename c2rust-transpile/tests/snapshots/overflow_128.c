// Overflow builtins mixing 128-bit and other integer types are rejected, so
// the `reject`-prefixed functions must be missing from the snapshot.
// See https://github.com/immunant/c2rust/issues/1878.

int reject_uint128_operands(unsigned __int128 a, unsigned __int128 b,
                            unsigned long long *out)
{
    return __builtin_add_overflow(a, b, out);
}

int reject_uint128_result(int a, int b, unsigned __int128 *out)
{
    return __builtin_mul_overflow(a, b, out);
}

int reject_int128_operand(__int128 a, long long b, long long *out)
{
    return __builtin_sub_overflow(a, b, out);
}

int supported_uint128(unsigned __int128 a, unsigned __int128 b,
                      unsigned __int128 *out)
{
    return __builtin_add_overflow(a, b, out);
}

int supported_int128(__int128 a, __int128 b, __int128 *out)
{
    return __builtin_mul_overflow(a, b, out);
}
