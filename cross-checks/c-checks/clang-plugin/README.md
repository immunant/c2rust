# Clang plugin for crosschecking on C programs

This is a cross-check inserter for C programs implemented as a clang compiler plugin.

## Building and running the plugin

1. To Build this program, which depends on llvm-6.0.0, you must use a matching clang version, _clang-6.0_.

   `$C2RUST/scripts/build_translator.py --with-clang`, this command will build clang in the build process. 

2. Build `libfakechecks` (optional, useful for testing):
   ```bash
    $ cd $C2RUST/cross-checks/libfakechecks
    $ make all
   ```

3. Build the clang plugin using the build script:

   `$C2RUST/scripts/build_cross_checks.py`

   (We use our own built clang to build and run the plugin due to this [bug](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=862328))

4. Also note, that the clang you're using must not be a debug version. 

## Testing

This plugin could be tested in this directory by running `make test.bin`. Note that `PLUGIN_CC` should be changed to the clang from the repository, or another custom clang-6.0.


