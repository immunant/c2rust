This is a zeroing replacement for the `malloc` family of functions.
It intercepts all memory allocation requests and returns zeroed memory.
To use this library with your application, preload it using `LD_PRELOAD`:
```
 $ env LD_PRELOAD=.../libzero_malloc.so <program> <arguments>
```

The library can be built with the following features:
 * `debug-print` prints each allocation request to standard error, for
   debugging.
