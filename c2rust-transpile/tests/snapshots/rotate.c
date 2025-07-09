unsigned char rotate_left_8(unsigned char x) {
    return __builtin_rotateleft8(x, 1);
}

unsigned short rotate_left_16(unsigned short x) {
    return __builtin_rotateleft16(x, 2);
}

unsigned rotate_left_32(unsigned x) {
    return __builtin_rotateleft32(x, 3);
}

unsigned char rotate_right_8(unsigned char x) {
    return __builtin_rotateright8(x, 1);
}

unsigned short rotate_right_16(unsigned short x) {
    return __builtin_rotateright16(x, 2);
}

unsigned rotate_right_32(unsigned x) {
    return __builtin_rotateright32(x, 3);
}
