#include <alloca.h>

int TRUE = 1;

int alloca_sum(int val1, int val2) {
    int* alloca1;
    int* alloca2;

    if (TRUE) {
        alloca1 = alloca(sizeof(int));
        *alloca1 = val1;
    }

    if (TRUE) {
        alloca2 = alloca(sizeof(int));
        *alloca2 = val2;
    }

    return *alloca1 + *alloca2;
}

