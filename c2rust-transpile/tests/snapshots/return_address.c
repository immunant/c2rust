void *direct_caller(void) {
    return __builtin_return_address(0);
}

void *deeper_caller(void) {
    return __builtin_return_address(1);
}
