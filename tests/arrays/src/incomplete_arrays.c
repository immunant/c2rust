#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

extern uint32_t SOME_INTS[];

bool check_some_ints(void) {
        return SOME_INTS[0] == 2
            && SOME_INTS[1] == 0
            && SOME_INTS[2] == 1
            && SOME_INTS[3] == 8;
}

struct sized_array {
        size_t n;
        unsigned elts[];
};

struct sized_array *new_sized_array(size_t n) {
        const size_t sz = sizeof(struct sized_array) + n * sizeof(unsigned);
        struct sized_array *res = malloc(sz);
        res->n = n;
        return res;
}

// This test ensures that `struct sized_array *` isn't a fat
// pointer, which would cause the following argument `n` to
// be garbage
unsigned sized_array_sum_last_n(struct sized_array *a, size_t n) {

        unsigned acc = 0;
        for (size_t i = a->n - n; i < a->n; i++) {
                acc += a->elts[i];
        }
        return acc;
}

unsigned test_sized_array(void) {

        struct sized_array *a = new_sized_array(10);

        for (int i = 0; i < 10; i++) {
                a->elts[i] = i;
        }

        const unsigned sum = sized_array_sum_last_n(a, 4);

        free(a);

        return sum;
}

void entry2(const unsigned buffer_size, int buffer[])
{
    int arr[3] = {1,2,3};
    int (*p_arr)[] = &arr;
    int x = (*p_arr)[0];
    buffer[0] = x;

    const int carr[3] = {1,2,3};
    const int (*p_carr)[] = &carr;
    int cx = (*p_carr)[0];
    buffer[1] = cx;
}
