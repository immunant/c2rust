#include <stdbool.h>

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
}
