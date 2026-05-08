#define __same_type(a, b) __builtin_types_compatible_p(typeof(a), typeof(b))

typedef int int_t;

extern int incomplete_array[];
int fixed_array[5];

enum first_kind {
    FIRST_KIND_A,
};

enum second_kind {
    SECOND_KIND_A,
};

int same_unqualified_int = __builtin_types_compatible_p(int, const int);
int same_typedef = __builtin_types_compatible_p(int_t, int);
int same_typeof_int = __same_type(same_unqualified_int, same_typedef);
int same_array_type = __same_type(fixed_array, incomplete_array);
int different_pointer_type = __builtin_types_compatible_p(int *, const int *);
int different_enum_type = __builtin_types_compatible_p(enum first_kind, enum second_kind);

int choose_same(int x, long y) {
    return __builtin_choose_expr(__same_type(x, int), x, y);
}

int choose_different(int x, long y) {
    return __builtin_choose_expr(__same_type(y, int), x, 7);
}
