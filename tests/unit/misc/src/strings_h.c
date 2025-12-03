#include <string.h>

void setmem(unsigned size, int* buffer) {
    memset(buffer, 1, size * sizeof(*buffer));
}
