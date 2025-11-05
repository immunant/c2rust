#include <stdbool.h>
#include <stdint.h>

bool use_portable_type(uintptr_t len) { return len <= UINTPTR_MAX / 2; }
