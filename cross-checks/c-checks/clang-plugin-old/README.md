# Clang plugin for crosschecking on C programs

This is a cross-check inserter for C programs developed using clang plugin feature.
Current implementation inserts checks on function entry point and on arguments.

## Building and running the plugin

1. To compile the plugin, some flags need to be specified so the compiler can find the LLVM and Clang header files, etc. The Makefile provided does the job.Build path depends on your installation and Makefile needs to be modified accordingly.

2. Make sure the LLVM download is in your $(HOME)(Needs to be changed in Makefile if installed elsewehere) directory and as building LLVM in the source directory is not supported,create a new folder build in your $(HOME) and build both LLVM and Clang

3. Run make utility and crosschecker executable will be present in build directory ("/clang-plugin/build/"CrossChecker")

4. You can use the plugin to insert crosschecks on a C source with the command ./build/CrossChecker "filename.c"

## Testing

Values inserted by Clang plugin can be verified against GCC instrumentation method
(cyg_profile_func methods) by "build.py" script executes the clang plugin against
a source code on which cross-checks needs to inserted. The transformed source code
via plugin & gcc instrumented source is then linked against the shared library(libfakechecks.so)
and executed, outputs are copied on to a text files(i.e output.txt & plugin_output.txt) and then compared.
If there exists no difference between crosscheck values(Tag and Hash values) in the files the script will print "Test passed:Crosschecks inserted match for both methods" after execution

