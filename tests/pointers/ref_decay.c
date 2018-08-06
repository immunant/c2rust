#include <stdlib.h>

typedef struct {
    int *a;
    const int *b;
    void *c;
} ThreeFields;

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

    // RHS shouldn't decay in k, but decays m due to bitcast
    int *k = &i;
    void *m = &i;

    // First and second fields shouldn't decay, third does due to bitcast
    ThreeFields tf = {&i, &i, &i};

    int l[2];
    // Variadic functions need references decayed or else it won't compile.
    sscanf(k, "%u,%u", &l[0], &l[1]);
}
