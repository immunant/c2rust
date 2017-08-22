#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

uint32_t djb2_hash(const unsigned char *str) {
    uint32_t hash = 5381UL;
    int c;

    while (c = *str++)
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

void __cyg_profile_func_enter(void *, void *) __attribute__((no_instrument_function, visibility("default")));

void __cyg_profile_func_enter(void *func,  void *caller) {
    Dl_info func_info, caller_info;
    dladdr(func, &func_info);
    printf("Called %s@%p hash:%" PRIx32 "\n",
           func_info.dli_sname, func,
           djb2_hash(func_info.dli_sname));
    // TODO: compute djb2 hash of name, then call rb_xcheck
}
