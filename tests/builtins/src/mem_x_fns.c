#include <stdio.h>

void mem_x(const char src[4], char dest[4]) {
    __builtin_memcpy(dest, src, strlen(src)+1);
    __builtin_memchr(dest, 'a', strlen(src)+1);
    __builtin_memcmp(dest, src, strlen(src)+1);
    __builtin_memmove(dest, src, strlen(src)+1);
    __builtin_memset(dest, 'a', 2);
}

void* assume_aligned(void* ptr) {
    __builtin_unwind_init();
    return __builtin_assume_aligned(ptr, 8);
}
