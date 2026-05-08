void *direct_frame(void) {
    return __builtin_frame_address(0);
}

void *deeper_frame(void) {
    return __builtin_frame_address(1);
}
