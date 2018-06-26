# C2Rust

## Translation

The `ast-exporter` extracts from a C file the abstract syntax tree and type information produced by
Clang and serializes it into CBOR files. The `ast-importer` consumes these CBOR files and generates
Rust source code preserving the semantics (as understood under C99) of the initial C program.

The translated Rust files will not depend directly on each other like
normal Rust modules. They will export and import functions through the C
API. These modules can be compiled together into a single static Rust
library.

There are several [known limitations](https://github.com/immunant/c2rust/wiki/Known-Limitations-of-Translation)
in this translator. Some of these restrictions come from limitations of
Rust and others come from complexities of the features themselves. The
translator will attempt to skip function definitions that use
unsupported features.

### Building

These two projects have some large dependencies (namely parts of LLVM and Clang). If you've installed 
the necessary tools, the following should build `ast-exporter` and `ast-importer` and all of their
dependencies, automatically pulling them in if necessary.

Building from scratch takes on the order of 30 minutes. This script works on at least MacOS.

    $ ./scripts/build_translator.py

On Ubuntu, ninja-build is required.

To manually build the `ast-exporter`, check out [these build instructions][0]. To manually build the
`ast-importer`, check out [its README](ast-importer/README.md).

### Testing

Tests are found in the [`tests`](tests) folder. If both the `ast-exporter` and `ast-importer` are
built, you should be able to run the tests with

    $ ./scripts/test_translator.py tests

This basically tests that the original C file and translated Rust file produce the same output when
compiled and run. More details about tests are in [this README](tests/README.md).

 [0]: docs/building-ast-exporter.md

### Using the translator

The C2Rust translation process relies use Clang to parse and type-check
input C files. For Clang to do this it needs to know information that is
passed in via command-line flags. This information can be found in an
automatically generated `compile_commands.json`.

The `compile_commands.json` file can be automatically create using
either `cmake` or `Bear`.

#### Generating `compile_commands.json` with `cmake`

When creating the initial build directory with cmake specify
`-DMAKE_EXPORT_COMPILE_COMMANDS=1`. This only works on projects
configured to be built by cmake. This works on Linux and MacOS.

    $ mkdir build
    $ cd build
    $ cmake -DMAKE_EXPORT_COMPILE_COMMANDS=1 ..

#### Generating `compile_commands.json` with `bear`

When building on Linux, *Bear* is automatically build by the
`build_translator.py` script and installed into the `dependencies`
directory.

    $ ./configure CC=clang
    $ bear make

#### Translating source files

The `transpile.py` script will automatically translate all of the C
source files mentioned in the previously generated
`compile_commands.json`.

    $ scripts/transpile.py ./compile_commands.json
