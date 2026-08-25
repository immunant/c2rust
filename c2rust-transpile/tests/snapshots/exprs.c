#include <stddef.h>

int puts(const char *str);

static int side_effect(){
    puts("the return of side effect");
    return 0;
}

static int* lvalue_side_effect(){
    puts("the return of side effect");
    static int VAL = 42;
    return &VAL;
}

void unary_without_side_effect(){
    int i = 5;
    -i;
    +i;
    ~i;
    !i;
    &i;
    *&i;
    i++;
    i--;
    --i;
    ++i;
}

void unary_with_side_effect(){
    char *arr[1] = {0};

    -side_effect();
    +side_effect();
    ~side_effect();
    !side_effect();
    &""[side_effect()];
}

void inc_decl_with_rvalue_side_effect() {
    int arr[1] = {0};

    // Increment/decrement, expression value not used
    ++arr[side_effect()];
    --arr[side_effect()];
    arr[side_effect()]++;
    arr[side_effect()]--;

    // Increment/decrement, expression value is used
    int pre_inc = ++arr[side_effect()];
    int pre_dec = --arr[side_effect()];
    int post_inc = arr[side_effect()]++;
    int post_dec = arr[side_effect()]--;
}

void inc_decl_with_lvalue_side_effect() {
    // Increment/decrement, expression value not used
    ++*lvalue_side_effect();
    --*lvalue_side_effect();
    (*lvalue_side_effect())++;
    (*lvalue_side_effect())--;

    // Increment/decrement, expression value is used
    int pre_inc = ++*lvalue_side_effect();
    int pre_dec = --*lvalue_side_effect();
    int post_inc = (*lvalue_side_effect())++;
    int post_dec = (*lvalue_side_effect())--;
}

void unsigned_compound_desugaring(void) {
    enum E { EA };

    int i = 0;
    unsigned int u = 0;
    enum E e = EA;
    e += u;
    i += u;
}

typedef int int_t;

void cast_literals(void) {
    int i = 1L;
    int_t it = 1L;
    int i_neg = -1L;
    int_t it_neg = -1L;
}

void compound_literal(){
    /// https://github.com/immunant/c2rust/issues/1234
    int i = (enum {A, B, C}){1};
}

void statement_expr() {
    ({
        puts("should execute");
        return;
    });

    puts("should be unreachable!");
}

void pointer_arithmetic(void) {
    int i1[] = { 0, 1 };
    int i2[] = { 10, 11 };
    int *p1 = i1;
    int *p2 = i2;
    ptrdiff_t diff = p1 - p2;
    int diff_int = p1 - p2;
}

void assign_result(void) {
    unsigned long l = 0;
    size_t s1 = l = 1;
    size_t s2 = l += 2;
}

void bypass_cast_with_typedef(void) {
    // Primitive
    int i = 0;
    int_t t_implicit = i;
    int_t t_explicit = (int_t) i;

    // Pointer
    int *pi = 0;
    int_t *pt_implicit = pi;
    int_t *pt_explicit = (int_t *) pi;

    // Pointer to array
    int (*pai)[2] = 0;
    int_t (*pat_implicit)[2] = pai;
    int_t (*pat_explicit)[2] = (int_t (*)[2]) pai;

    // Pointer to function
    int (*pfi)(int) = 0;
    int (*pftp_implicit)(int_t) = pfi;
    int (*pftp_explicit)(int_t) = (int (*)(int_t)) pfi;
    int_t (*pftr_implicit)(int) = pfi;
    int_t (*pftr_explicit)(int) = (int_t (*)(int)) pfi;
}
