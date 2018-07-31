#include <stdlib.h>

void f(int *a, const int *b) {}
void bar(const int *a) {
    int arr[3] = {1,2,3};
    int (*p_arr)[] = &arr;

    f(&arr[0], a);
}
void bitcast(void *a) {}
void foobar(unsigned int *a) {}

void calls_all() {
    int i = 1;
    const int j = 2;

    f(&i, &i);
    bar(&j);
    foobar(&i);

    // Bitcast required; should ref decay or else won't compile.
    bitcast(&i);

    int *k = &i;
    int l[2];

    // Variadic functions need references decayed or else it won't compile.
    sscanf(k, "%u,%u", &l[0], &l[1]);
}
