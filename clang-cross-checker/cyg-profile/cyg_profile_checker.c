#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdio.h>

void __cyg_profile_func_enter (void *, void *) __attribute__((no_instrument_function, visibility("default")));

void __cyg_profile_func_enter (void *func,  void *caller) {
    Dl_info func_info, caller_info;
    dladdr(func, &func_info);
    dladdr(caller, &caller_info);
    printf("Called %s@%p from %s@%p\n",
           func_info.dli_sname, func,
           caller_info.dli_sname, caller);
    // TODO: compute djb2 hash of name, then call rb_xcheck
}
