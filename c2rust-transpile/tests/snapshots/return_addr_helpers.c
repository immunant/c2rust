extern void *raw_addr(void);

void *extract(void) {
    return __builtin_extract_return_addr(raw_addr());
}

void *frob(void) {
    return __builtin_frob_return_addr(raw_addr());
}

void discard_extract(void) {
    __builtin_extract_return_addr(raw_addr());
}

void discard_frob(void) {
    __builtin_frob_return_addr(raw_addr());
}
