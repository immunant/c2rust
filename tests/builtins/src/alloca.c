#include <stdio.h>
#include <string.h>

int one = 1;

int alloca_hello(void) {
    char *p;
    if (one) {
        p = __builtin_alloca(100);
    }
    strcpy(p, "Hello, world!");
    puts(p);
    return 0;
}
