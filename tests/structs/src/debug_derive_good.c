//! derive_debug

#include <stdarg.h>

typedef struct {
    int a;
} S1;

typedef struct {
    va_list v;
} S2;

S1 *kS1;
S2 *kS2;
