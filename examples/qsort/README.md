# qsort

This tiny project provides an example of how to use
CMake to build a C project and to generate the clang
"compile_commands.json" file which is used by tools
like the c2rust-ast-exporter.

Build with the following commands:
```
$ mkdir ../build
$ cd ../build
$ cmake ../qsort
$ cmake --build .
$ translate.py FULLPATHTO/qsort.c
```
