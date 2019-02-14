#include <stddef.h>

typedef struct {
    int a;
    float mod[3];
} use;

size_t get_offset(size_t idx) {
    use u; // FIXME: type gets pruned out otherwise
    return offsetof(use, mod[idx]);
}

struct yield {
    int a;
    float mod[3];
};

size_t get_offset2(size_t idx) {
    struct yield u; // FIXME: type gets pruned out otherwise
    return offsetof(struct yield, mod[idx]);
}
