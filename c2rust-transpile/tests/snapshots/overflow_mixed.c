// Overflow builtins with operand/result types that do not share one Rust
// type compute in i128 and check that the result fits via `try_from`.

int mixed_types(long long a, int b, unsigned long long *out)
{
    return __builtin_add_overflow(a, b, out);
}

int narrow_result(long long a, long long b, int *out)
{
    return __builtin_mul_overflow(a, b, out);
}
