void ptr(void) {
    const char *_s = "hello";
}

void array_deduced_length(void) {
    char _s[] = "hello";
}

void array(void) {
    char _s[10] = "hello";
}

void int_array_extra_braces(void) {
    int _a[3][10] = {{1, 2, 3}};
}

void ptr_extra_braces(void) {
    const char *_s = {"hello"};
}

void array_extra_braces(void) {
    const char _s[10] = {"hello"};
}

void array_of_ptrs(void) {
    const char* _s[3] = {"hello"};
}

void array_of_arrays(void) {
    char _s[3][10] = {"hello"};
}

void array_of_arrays_static(void) {
    static const char _S[3][10] = {"hello"};
}

void array_of_arrays_static_struct(void) {
    struct s {
        char entries[3][10];
    };

    static const struct s _S = {
        {"hello"},
    };
}

// From curl.
void curl_alpn_spec(void) {
    #define ALPN_ENTRIES_MAX 3
    #define ALPN_NAME_MAX    10
    #define ALPN_HTTP_1_1    "http/1.1"

    struct alpn_spec {
        char     entries[ALPN_ENTRIES_MAX][ALPN_NAME_MAX];
        // TODO Changed from `size_t` since `size_t` is still platform-dependent for now.
        // Undo once #1266 lands.
        unsigned count; /* number of entries */
    };

    static const struct alpn_spec _ALPN_SPEC_H11 = {
        { ALPN_HTTP_1_1 }, 1
    };
}
