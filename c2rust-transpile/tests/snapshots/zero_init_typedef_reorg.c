typedef struct Stat {
    int mode;
    long size;
} STAT_T;

/* The implicit zero-initializer for the `st` field is the first one created
 * for `struct Stat`, so it populates the cache. Because the field is spelled
 * with the typedef, the cached expression is rendered as `STAT_T { ... }`. */
struct OuterMain {
    int x;
    STAT_T st;
};

static void before_include(void) {
    struct OuterMain o = {0};
    (void)o;
}

#include "zero_init_typedef_reorg.h"

int main(void) {
    before_include();
    after_include();
    return 0;
}
