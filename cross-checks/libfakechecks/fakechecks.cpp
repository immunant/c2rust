#include <cstdio>

extern "C"
void rb_xcheck(unsigned long item) {
    fprintf(stderr, "XCHECK:%lu/0x%08lx\n", item, item);
}
