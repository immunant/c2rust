#include <stdbool.h>
#include <stdlib.h>

enum E {
    A,
};

void scalar_init() {
    bool b = { true };
    char c = { 'c' };
    int i = { 42 };
    long l = { 42l };
    float f = { 42.0f };
    double d = { 42.0 };
    long double ld = { 42.0l };
    // Doesn't work on MacOS, so avoid it for now so this test doesn't become OS-specific.
    // __float128 q = { 42.0q };
    enum E e = { A };
    int *p = { &i };

// Empty initializer lists for scalars was only added in C23 and clang 17.
#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311L) || (defined(__clang__) && (__clang_major__ >= 17))
    bool eb = {};
    char ec = {};
    int ei = {};
    long el = {};
    float ef = {};
    double ed = {};
    long double eld = {};
    // __float128 eq = {};
    // enums don't allow empty init lists in C
    int *ep = {};
#else
    bool eb;
    char ec;
    int ei;
    long el;
    float ef;
    double ed;
    long double eld;
    // __float128 eq;
    // enums don't allow empty init lists in C
    int *ep;
#endif
}
