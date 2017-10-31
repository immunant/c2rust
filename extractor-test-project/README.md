This tiny project provides an example of how to use
CMake to build a C project and to generate the clang
"compile_commands.json" file which is used by tools
like the ast-extractor.

$ mkdir ../build
$ cd ../build
$ cmake ../extractor-test-project
$ cmake --build .
$ ast-extractor FULLPATHTO/qsort.c
