int ffs(int a) {
    return __builtin_ffs(a);
}

int ffsl(long a) {
    return __builtin_ffsl(a);
}

int ffsll(long long a) {
    return __builtin_ffsll(a);
}

int isfinite(double a) {
    return __builtin_isfinite(a);
}

int isnan(double a) {
    return __builtin_isnan(a);
}

int isinf_sign(double a) {
    return __builtin_isinf_sign(a);
}