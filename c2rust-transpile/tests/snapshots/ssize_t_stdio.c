/* `ssize_t` is declared here only via stdio.h (POSIX.1-2008), not via
 * sys/types.h. Both declaration sites must map to the same Rust type, or
 * translation units within one crate disagree about what `ssize_t` is. */
#include <stdio.h>

ssize_t zero_len(void) {
    return 0;
}
