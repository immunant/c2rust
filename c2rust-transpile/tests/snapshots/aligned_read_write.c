#include <stdint.h>

typedef __attribute__((aligned(1))) uint32_t unaligned_uint32;

uint32_t unaligned_read(const void* ptr) {
    return *((const unaligned_uint32*)ptr);
}

uint32_t aligned_read(const void* ptr) {
    return *((const uint32_t*)ptr);
}

void unaligned_write(void* ptr, uint32_t value) {
    *((unaligned_uint32*)ptr) = value;
}

void aligned_write(const void* ptr, uint32_t value) {
    *((uint32_t*)ptr) = value;
}
