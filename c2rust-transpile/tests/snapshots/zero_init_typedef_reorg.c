typedef struct Stat {
    int mode;
    long size;
} STAT_T;

static void before_include(void) {
    STAT_T st = {0};
    (void)st;
}

#include "zero_init_typedef_reorg.h"

int main(void) {
    before_include();
    after_include();
    return 0;
}
