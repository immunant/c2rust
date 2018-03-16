# Clang plugin for crosschecking on C programs

This is a cross-check inserter for C programs developed using the clang plugin feature.

## Building and running the plugin

1. To Build this program, which is built with llvm-6.0.0, you must use a matching clang version, _clang.6.0.0_.

   `./scripts/build_translator.py --with-clang`, this command will build clang in the build process. 

2. Create a build directory, then 

   ```bash
    $ cd build 
    $ cmake -G Ninja -DClang_DIR=../../../dependencies/llvm-6.0.0/build.machine_name/lib/cmake/clang ../ 
    $ ninja
    ```

   (The reason we set the clang directory to one installed with the build, is due to this [bug](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=862328))

3. Also note, that the clang you're using must not be a debug version. 

## Testing

This plugin could be tested in this directory with _test.c_, notice `PLUGIN_CC` should be changed to be the clang built, or a path clang-6.0.0 
