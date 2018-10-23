#include <stdio.h>

typedef unsigned long* ulp;

ulp foo(void) {
    return NULL;
}

// GH #99: Previously bitcasts were ignored for int* to const int*
// messing up type inference in read_volatile type conversion
int simple(int const * *x, int * volatile *y) {
    return *x == *y;
}

void entry(const unsigned int buffer_size, int buffer[]) {
    char *test = {"string"};
    buffer[0] = test[0];
    buffer[1] = test[1];
    buffer[2] = test[2];
    buffer[3] = test[3];
    buffer[4] = test[4];

    // GH #89: Previously a null ptr return value would cause a
    // typedef'd pointer type to cause the function to silently
    // not generate any definition
    foo();
}
