# Clang plugin for crosschecking on C programs

This is a cross-check inserter for C programs implemented as a clang compiler plugin.

## Building and running the plugin

1. To build the plugin, which depends on llvm-6.0, you must use a matching clang version, _clang-6.0_.
   ```bash
    $ ../../../scripts/build_translator.py --with-clang
   ```
   Also note that the clang you're using must not be a debug version.

2. Build `libfakechecks` (optional, useful for testing):
   ```bash
    $ cd ../../libfakechecks
    $ make all
   ```

3. Build the clang plugin using the build script:
   ```bash
    $ ../../../scripts/build_cross_checks.py
   ```
   (We use our own built clang to build and run the plugin due to this [bug](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=862328))

4. To compile code using the plugin, either wrap the compilation command with the `cc_wrapper.sh` script from this directory:
  ```bash
    $ cc_wrapper.sh .../clang-6.0 .../CrossChecks.so <rest of command line...>
  ```
  or add the following arguments manually to the clang command line, e.g., using `CFLAGS`:
  `-Xclang -load -Xclang .../CrossChecks.so -Xclang -add-plugin -Xclang crosschecks`
  and link against `libruntime.a`.
  In either approach, the target binary must be linked against one of the `rb_xcheck` implementation libraries: `libfakechecks.so` or `libclevrbuf.so`.

## Testing

This plugin could be tested in this directory by running `make test`. Note that `PLUGIN_CC` should be changed to the clang from the repository, or another custom clang-6.0.


