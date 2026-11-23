// `__float128` is only available on some targets, such as x86_64.
__float128 static_nansf128 = __builtin_nansf128("");

void f128_builtins(void) {
    __float128 inf = __builtin_inff128();
    __float128 huge = __builtin_huge_valf128();

    __float128 q = __builtin_nanf128("");
    __float128 s = __builtin_nansf128("");
    __float128 q1 = __builtin_nanf128("1");
    __float128 s1 = __builtin_nansf128("0x1f");
}
