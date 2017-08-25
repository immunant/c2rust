# Cyg-profile cross-checker

This is a simple cross-check inserter for C/C++ code that uses the `-finstrument-functions` option provided by gcc/clang.
When this option is passed on the compiler command line, the compiler instruments all function entry and exit points with calls
to the `__cyg_profile_func_enter` and `__cyg_profile_func_exit` functions.
We provide the `libcrosscheck.so` library which implements a `__cyg_profile_func_enter` function that perform a simple cross-check on its caller.

## Usage

To automatically add the cross-checks, compile your program with the `-finstrument-functions -rdynamic -lcrosscheck` options.
The `-rdynamic` adds all program symbols to the dynamic symbol table, which `dladdr` uses to find the name of a function given its address.
The cross-checks are actually implemented in `libclevrbuf.so` from the ReMon repository, so that library also needs to be linked in or pre-loaded with `LD_PRELOAD`.
