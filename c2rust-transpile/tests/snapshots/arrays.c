#include <stdlib.h>

char static_char_array[] = "mystring";
char *static_char_ptr = "mystring";
void *static_void_ptr = (void*) static_char_array;

typedef int int_t;

void entry(void) {
    int int_2d[1][1] = { 1 };
    int_2d[0][0] += 9;
    int int_empty_init[16] = {};
    int_empty_init[15] += 9;
    int int_too_long[2] = { 1, 2, 3 };
    int int_zero[0] = { 1234 };
    int int_too_short[16] = {0};
    int_too_short[15] += 9;

    int_t override_ty[] = { 1 };
    int_t override_ty_neg[] = { -1 };

    struct {char* x; int y;} struct_init_too_short[1] = {};
    struct_init_too_short[0].y += 9;
    struct {short; int y;} struct_init_too_long[1] = { { 1, 2 } };
    struct_init_too_long[0].y += 9;

    char char_with_string[] = "abc";
    char char_with_chars[] = {'d','e','f'};
    char char_with_ints[2] = {1};
    char char_with_init[] = {"abcd"};
    char char_too_long[3] = "abcde";
    char char_too_short[20] = "abc";

    int *int_var_ptr = int_empty_init;
    int (*int_var_array_ptr)[16] = &int_empty_init;
    char *char_var_ptr = char_with_string;
    char (*char_var_array_ptr)[4] = &char_with_string;
    const char *const_char_lit_ptr = "abc";
    const char (*const_char_lit_array_ptr)[4] = &"abc";
    char *char_lit_ptr = "abc";
    char (*char_lit_array_ptr)[4] = &"abc";

    // Test that we can get the address of the element past the end of the array
    char *past_end = &static_char_array[sizeof(static_char_array)];
    past_end = &static_char_ptr[8];
}

void short_initializer() {
    int empty_brackets[16] = {};
    int brackets_with_zero[16] = {0};
    int brackets_with_one[4] = {1};

    // excess elements
    int excess_elements_1[2] = { 1, 2, 3 };
    int excess_elements_2[0] = { 1234 };

    struct {short x; int y;} single_struct[1] = { { 1, 2 } };
    struct {short x; int y;} many_struct[3] = { { 1, 2 } };
}
