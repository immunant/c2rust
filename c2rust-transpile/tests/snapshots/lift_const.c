// Instead of `#include <stdio.h>` to avoid Linux vs. macOS divergence.
int printf(const char *restrict format, ...);

void lift_const(int argc, char **argv) {
    // jump past initializer to trigger variable rebinding in translated code
    if (argc == 3) {
        goto three_args;
    }

    const int starts_with_a = *argv[0] == 'a';
    if (starts_with_a) {
        printf("  (arg 0 starts with 'a')\n");
    }

three_args:
    if (argc == 3) {
        printf("  (argc was 3)\n");
    }
}
