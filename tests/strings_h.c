#include <string.h>

void entry(unsigned size, int* buffer) {
    memset(buffer, 1, size * sizeof(*buffer));
}
