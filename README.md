# C2Rust

## Translation

The `ast-extractor` extracts from a C file the abstract syntax tree and type information produced by
Clang and serializes it into CBOR files. The `ast-importer` consumes these CBOR files and generates
Rust source code preserving the semantics (as understood under C99) of the initial C program.

### Building

These two projects have some large dependencies (namely parts of LLVM and Clang). If you've installed 
the necessary tools, the following should build `ast-extractor` and `ast-importer` and all of their
dependencies, automatically pulling them in if necessary.

Building from scratch takes on the order of 30 minutes. This script works on at least MacOS.

    $ ./scripts/build_translation.py

To manually build the `ast-extractor`, check out [this Wiki page][0]. To manually build the
`ast-importer`, check out [its README](ast-importer/README.md).

### Testing

Tests are found in the [`tests`](tests) folder. If both the `ast-extractor` and `ast-importer` are
built, you should be able to run the tests with

    $ ./scripts/test_translation.py tests

This basically tests that the original C file and translated Rust file produce the same output when
compiled and run. More details about tests are in [this README](tests/README.md).

 [0]: https://github.com/GaloisInc/C2Rust/wiki/Building-AST-extractor

