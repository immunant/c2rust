#include <stdlib.h>

typedef struct Foo {
    unsigned a;
    int b;
    float c;
} Foo;
Foo params[] = {
    { 1, 0, 1.3 },
    { 1, 0, 1.2 },
};

// These should initialize fine:
int dont_section_a = 0;
int dont_section_b = -1;
int *dont_section_c = &dont_section_a;
int *dont_section_d = &dont_section_d;
Foo dont_section_foo;
unsigned dont_section_me = 1 + 1;

// These should be initialized via sections:
unsigned section_me = -1U;
int section_me2 = NULL;
unsigned section_me3 = 1U + 2U;
unsigned section_me4 = 1 + 1U;
unsigned section_me5 = 1U + 1;
Foo section_foo_b_field = {1, -1U, 1.2};
const unsigned int section_num_params = sizeof(params) / sizeof(params[0]);
const size_t if_expr = sizeof(size_t) == 4 ? 30 : 31;

size_t fn_scoped_static_init(void) {
    static size_t sectioned_scoped_init = &section_me;
    static unsigned not_sectioned = 1;
    return sectioned_scoped_init;
}
