#include <cstdio>

extern "C"
void rb_xcheck(unsigned long item) {
    fprintf(stderr, "XCHECK:%lu\n", item);
}
