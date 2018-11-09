This tiny project provides an example of how to use
CMake to build a C project and to generate the clang
"compile_commands.json" file which is used by tools
like the c2rust-ast-exporter.

```
$ mkdir ../build
$ cd ../build
$ cmake ../qsort
$ cmake --build .
$ c2rust-ast-exporter FULLPATHTO/qsort.c
```
