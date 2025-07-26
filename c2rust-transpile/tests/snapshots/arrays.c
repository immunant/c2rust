#include <stdlib.h>

static char simple[] = "mystring";
static char *foo = "mystring";

void entry(void) {
    int arr[1][1] = { 1 };
    arr[0][0] += 9;

    int arr2[16] = {};
    arr2[15] += 9;

    struct {char* x; int y;} arr3[1] = {};
    arr3[0].y += 9;

    int arr4[16] = {0};
    arr4[15] += 9;

    struct {short; int y;} arr5[1] = { { 1, 2 } };
    arr5[0].y += 9;

    // excess elements
    int arr6[2] = { 1, 2, 3 };
    int arr7[0] = { 1234 };

    char abc[] = "abc";

    char def[] = {'d','e','f'};

    char part[2] = {1};

    char *abcptr = "abc";

    char init[] = {"abcd"};

    char too_long[3] = "abcde";

    char too_short[20] = "abc";

// TODO re-enable after #1266 adds portable support for translating `wchar_t`.
#if 0
    wchar_t wide1[] = L"x";

    wchar_t wide2[3] = L"x";

    wchar_t wide3[1] = L"xy";
#endif

    // Test that we can get the address of the element past the end of the array
    char *past_end = &simple[sizeof(simple)];
    past_end = &foo[8];
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
