#include <stddef.h>

int puts(const char *str);

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

    // The result type is different from the expected type.
    unsigned long *unsigned_long_pointer = 0;
    size_t *size_pointer = unsigned_long_pointer + 1;
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

#define P ((int *)0)

void mut_to_const_ptr_cast(int cond, const int *c, int *m, int **mm) {
    // Implicit BitCast
    int bit_cast = c == m;
    const int* conditional = cond ? c : m;

    // Mutable pointer in macro
    int macro = c == P;

    // Mutable borrow
    const int *borrow = &cond;

    // Mutable array element
    const int *array[] = { m };

    // Other qualifiers differ, so it's no longer treated as mut-to-const
    const volatile int *vol = m;

    // Const coming through a typedef isn't seen by is_mut_to_const_pointer_cast
    typedef const int cint;
    cint *td[] = { m };

    // Only the outer pointer level gains const
    int *const *ptr_ptr = mm;
}

// Mutable borrow of global/static
int global;
const int *global_ptr = &global;
