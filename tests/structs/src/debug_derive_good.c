//! derive_debug

#include <stdarg.h>

typedef struct {
    int a;
} S1;

S1 kS1;

typedef struct {
    va_list v;
} S3;

S3 get_struct_containing_va_list() {
    S3 s;
    return s;
}