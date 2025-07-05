void f0(void) {
    const char *_s = "hello";
}

void f1(void) {
    char _s[] = "hello";
}

void f2(void) {
    char _s[10] = "hello";
}

void f3(void) {
    int _a[3][10] = {{1, 2, 3}};
}

void f4(void) {
    const char* _s[3] = {"hello"};
}

void f5(void) {
    char _s[3][10] = {"hello"};
}

void f6(void) {
    static const char _S[3][10] = {"hello"};
}

void f7(void) {
    struct s {
        char entries[3][10];
    };

    static const struct s _S = {
        {"hello"},
    };
}

#include <stdlib.h>

// From curl.
void f8(void) {
    #define ALPN_ENTRIES_MAX 3
    #define ALPN_NAME_MAX    10
    #define ALPN_HTTP_1_1    "http/1.1"

    struct alpn_spec {
        char    entries[ALPN_ENTRIES_MAX][ALPN_NAME_MAX];
        size_t  count; /* number of entries */
    };

    static const struct alpn_spec _ALPN_SPEC_H11 = {
        { ALPN_HTTP_1_1 }, 1
    };
}
