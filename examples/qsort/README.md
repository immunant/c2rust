This tiny project provides an example of how to use
CMake to build a C project and to generate the clang
"compile_commands.json" file which is used by tools
like the ast-exporter.

```
$ mkdir ../build
$ cd ../build
$ cmake ../qsort
$ cmake --build .
$ ast-exporter FULLPATHTO/qsort.c
```
