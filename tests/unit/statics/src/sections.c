#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>

typedef struct Foo {
    unsigned a;
    int b;
    float c;
} Foo;
Foo params[] = {
    { 1, 0, 1.3 },
    { 1, 0, 1.2 },
};
typedef struct {
    int *bar;
} Bar;
typedef struct {
    int **f;
} stat;
typedef struct {
    int *f;
} stat2;
typedef struct {
    int *first;
    int **last;
} stat3;
typedef void (*void_fn_ptr)(void);

// These should initialize fine:
int dont_section_a = 0;
int dont_section_b = -1;
int *dont_section_c = &dont_section_a;
int *dont_section_d = &dont_section_d;
Foo dont_section_foo;
unsigned dont_section_me = 1 + 1;
Bar bar;
void *int_to_pointer = -1;
uintptr_t null_to_ptr_ty = (uintptr_t) NULL;

// These should be initialized via sections:
unsigned section_me = -1U;
int section_me2 = NULL;
unsigned section_me3 = 1U + 2U;
unsigned section_me4 = 1 + 1U;
unsigned section_me5 = 1U + 1;
Foo section_foo_b_field = {1, -1U, 1.2};
const unsigned int section_num_params = sizeof(params) / sizeof(params[0]);
const size_t if_expr = sizeof(size_t) == 4 ? 30 : 31;
stat s = {&bar.bar};
stat2 s2 = {&bar.bar};
static stat3 selfref = { NULL, &selfref.first };
void (*int_to_fn_ptr)(void) = -1;
void (*int_to_fn_ptr2)(int, ...) = -1;
void_fn_ptr int_to_fn_ptr3 = -1;
// Add a sectioned static here to test how the transpiler handles renames
uintptr_t sectioned_ptr = NULL;

size_t fn_scoped_static_init(void) {
    extern size_t fn_scoped_extern;
    static size_t sectioned_scoped_init = &section_me;
    static unsigned not_sectioned = 1;
    static uintptr_t sectioned_ptr = &section_me;
    section_me -= fn_scoped_extern;
    return sectioned_scoped_init;
}

size_t fn_scoped_extern = 1;

// For some reason (only when private/static) this static initializer
// which gets sectioned, doesn't generate a Block expr like ever other
// known static init, but an Array expr. If it is made public/non static,
// then it generates a Block expr
static size_t sectioned_array[] = {
    (size_t) &((Foo *)0)->a,
};

void use_sectioned_array() {
    size_t f = sectioned_array[0];
}

// Function-scoped statics referenced by a sectioned static's initializer
// must be hoisted along with it (https://github.com/immunant/c2rust/issues/1981):
typedef struct {
    const int *p;
    unsigned len;
} slice_ref;

unsigned fn_scoped_static_hoist(void) {
    static const int local_static_arr[] = { 1, 2, 3 };
    /* unsigned division => sectioned */
    static const slice_ref local_static_s[] = {
        { local_static_arr, sizeof(local_static_arr) / sizeof(local_static_arr[0]) },
    };
    static uintptr_t sectioned_ptr = &section_me3;
    return local_static_s[0].p[0] + local_static_s[0].len;
}

int fn_scoped_static_chain(void) {
    static int chain_a[] = { 5, 6, 7 };
    static int *chain_b = chain_a;
    /* pointer arithmetic => sectioned; hoists chain_b, then chain_a */
    static int **chain_c = &chain_b + 0;
    return **chain_c + chain_a[1];
}
