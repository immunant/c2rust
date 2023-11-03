#include <stdarg.h>

typedef struct {
    int a;
} S1;

typedef struct {
    va_list v;
} S3;

S1 kS1;
S3 kS3;