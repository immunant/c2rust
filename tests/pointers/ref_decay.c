#include <stdlib.h>

void f(int *a, const int *b) {}
void bar(const int *a) {
    int arr[3] = {1,2,3};
    int (*p_arr)[] = &arr;

    f(&arr[0], a);
}
void baz(void *a) {}
void foobar(unsigned int *a) {}

void calls_all() {
    int i = 1;
    const int j = 2;

    f(&i, &i);
    bar(&j);
    baz(&i);
    foobar(&i);
}
