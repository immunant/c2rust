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

extern void rb_xcheck(unsigned long) __attribute__((weak));

void __cyg_profile_func_enter(void *func,  void *caller) {
    Dl_info func_info, caller_info;
    dladdr(func, &func_info);
    const char *func_name = func_info.dli_sname;
    uint32_t func_hash = djb2_hash(func_name);
    if (rb_xcheck)
        rb_xcheck(func_hash);
}
