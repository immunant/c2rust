#include <stdlib.h>

void entry(const unsigned buffer_size, int buffer[const])
{
    int arr[1][1] = { 1 };

    arr[0][0] += 9;

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
}
