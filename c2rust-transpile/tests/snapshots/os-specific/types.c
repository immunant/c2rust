#include <stdlib.h>
#include <stdint.h>
#include <stddef.h> // for ptrdiff_t and ssize_t on Linux
#include <sys/types.h> // for ssize_t on macOS

int intvar = 0;

size_t sizevar = 0;
ssize_t ssizevar = 0;
intptr_t intptrvar = 0;
uintptr_t uintptrvar = 0;
ptrdiff_t ptrdiffvar = 0;

uint8_t uint8var = 0;
uint16_t uint16var = 0;
uint32_t uint32var = 0;
uint64_t uint64var = 0;
int8_t int8var = 0;
int16_t int16var = 0;
int32_t int32var = 0;
int64_t int64var = 0;

intmax_t maxvar = 0;
uintmax_t umaxvar = 0;
