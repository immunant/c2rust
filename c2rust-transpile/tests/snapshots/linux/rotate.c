unsigned long long rotate_left_64(unsigned long long x) {
    return __builtin_rotateleft64(x, 4);
}

unsigned long long rotate_right_64(unsigned long long x) {
    return __builtin_rotateright64(x, 4);
}
