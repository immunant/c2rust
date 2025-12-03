#include <stdlib.h>

static char simple[] = "mystring";
static char *foo = "mystring";

void entry(const unsigned buffer_size, int buffer[const])
{
    int arr[1][1] = { 1 };
    arr[0][0] += 9;

    int arr2[16] = {};
    arr2[15] += 9;

    struct {char* x; int y;} arr3[1] = {};
    arr3[0].y += 9;

    int i = 0;

    char abc[] = "abc";
    buffer[i++] = abc[0];
    buffer[i++] = abc[1];
    buffer[i++] = abc[2];
    buffer[i++] = abc[3];

    char def[] = {'d','e','f'};
    buffer[i++] = def[0];
    buffer[i++] = def[1];
    buffer[i++] = def[2];

    char part[2] = {1};
    buffer[i++] = part[0];
    buffer[i++] = part[1];

    char *abcptr = "abc";
    buffer[i++] = abcptr[0];
    buffer[i++] = abcptr[1];
    buffer[i++] = abcptr[2];
    buffer[i++] = abcptr[3];

    char init[] = {"abcd"};
    buffer[i++] = init[0];
    buffer[i++] = init[1];
    buffer[i++] = init[2];
    buffer[i++] = init[3];

    char too_long[3] = "abcde";
    buffer[i++] = too_long[0];
    buffer[i++] = too_long[1];
    buffer[i++] = too_long[2];

    char too_short[20] = "abc";
    buffer[i++] = too_short[0];
    buffer[i++] = too_short[1];
    buffer[i++] = too_short[2];
    buffer[i++] = too_short[3];
    buffer[i++] = too_short[4];
    buffer[i++] = too_short[5];
    buffer[i++] = too_short[6];

    wchar_t wide1[] = L"x";
    buffer[i++] = wide1[0];
    buffer[i++] = wide1[1];

    wchar_t wide2[3] = L"x";
    buffer[i++] = wide2[0];
    buffer[i++] = wide2[1];
    buffer[i++] = wide2[2];

    wchar_t wide3[1] = L"xy";
    buffer[i++] = wide3[0];

    buffer[i++] = simple[0];
    buffer[i++] = simple[1];
    buffer[i++] = simple[2];
    buffer[i++] = simple[3];
    buffer[i++] = simple[4];
    buffer[i++] = simple[5];
    buffer[i++] = simple[6];
    buffer[i++] = simple[7];

    buffer[i++] = foo[0];
    buffer[i++] = foo[1];
    buffer[i++] = foo[2];
    buffer[i++] = foo[3];
    buffer[i++] = foo[4];
    buffer[i++] = foo[5];
    buffer[i++] = foo[6];
    buffer[i++] = foo[7];

    // Test that we can get the address of the element past the end of the array
    char *past_end = &simple[sizeof(simple)];
    past_end = &foo[8];
}
