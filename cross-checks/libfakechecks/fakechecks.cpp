#include <cstdio>
#include <cstddef>
#include <cstdint>

extern "C"
void rb_xcheck(uint8_t tag, uint64_t item) {
    fprintf(stderr, "XCHECK(%hhd):%lu/0x%08lx\n", tag, item, item);
}
