#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int *a;
    const int *b;
    void *c;
} ThreeFields;

typedef struct {
    int i;
} OneInt;

void f(int *a, const int *b) {}
void bar(const int *a) {
    int arr[3] = {1,2,3};
    int (*p_arr)[] = &arr;

    f(&arr[0], a);
}
void bitcast(void *a) {}
void foobar(unsigned int *a) {}
void address_cast(unsigned long a) {}

int cmp_ref(int a, int **b) {
    // This should reference decay &a until
    // https://github.com/rust-lang/rust/issues/53772 is resolved
    // otherwise this would be a compilation error for &mut a != *b
    return &a != *b;
}

typedef struct Page {
    unsigned char *idx;
} Page;

void takesPtr(const unsigned char *p) {}

void calls_all(void) {
    int i = 1;
    const int j = 2;

    f(&i, &i);
    bar(&j);

    // Bitcast required; should ref decay or else won't compile.
    foobar(&i);
    bitcast(&i);

    // RHS shouldn't decay in k, but decays m due to bitcast
    int *k = &i;
    void *m = &i;

    // First and second fields shouldn't decay, third does due to bitcast
    ThreeFields tf = {&i, &i, &i};

    int l[2];
    // Variadic functions need references decayed or else it won't compile.
    sscanf(k, "%u,%u", &l[0], &l[1]);

    unsigned long ul = 0;
    address_cast((unsigned long)&ul);

    // Reference to struct field behind a struct ptr shouldn't ref decay
    OneInt *oi;

    int *n = &oi->i;

    // This should decay at the moment, because rust doesn't allow
    // a reference (lhs) to be compared to a ptr (rhs). (but the reverse works)
    // See https://github.com/rust-lang/rust/issues/53772
    if (&i == k) {}

    cmp_ref(i, &n);

    // wrapping_offset_from requires self to be a raw pointer,
    // and self params don't ref decay. So lhs should decay,
    // but not rhs
    int o = 1;
    int p = &o - &i;

    // Offset calls must have self decayed
    int *q = &o + 0;
    q = &o - 0;

    // Ptr index offset must have self decayed
    Page *r;
    takesPtr((&(r)->idx[0])[0]);
}
