#include <stddef.h>

typedef struct {
    int a;
    float mod[3];
} use;

size_t get_offset(size_t idx) {
    return offsetof(use, mod[idx]);
}

struct yield {
    int a;
    float mod[3];
};

size_t get_offset2(size_t idx) {
    return offsetof(struct yield, mod[idx]);
}
