# Clang plugin for crosschecking on C programs

This is a cross-check inserter for C programs implemented as a clang compiler plugin.

## Building and running the plugin

1. Build `libfakechecks` (optional, useful for testing):
   ```bash
    $ cd ../../libfakechecks
    $ make all
   ```

2. Build the clang plugin using the build script:
   ```bash
    $ ../../../scripts/build_cross_checks.py
   ```

3. To compile code using the plugin, either wrap the compilation command with the `cc_wrapper.sh` script from this directory:
  ```bash
    $ cc_wrapper.sh <path/to/clang> .../CrossChecks.so <rest of command line...>
  ```
  or add the following arguments manually to the clang command line, e.g., using `CFLAGS`:
  ```
  -Xclang -load -Xclang .../CrossChecks.so -Xclang -add-plugin -Xclang crosschecks
  ```
  and link against `libruntime.a`.
  In both cases, the target binary must then be linked against one of the `rb_xcheck` implementation libraries: `libfakechecks.so` or `libclevrbuf.so`.

## Testing

This plugin can be tested in this directory by running `make test`.
