# tinycc

## Checking out the tcc sources

In `path/to/examples/tinycc`, initialize the git submodule:

`git submodule update --init repo`

## Create a Makefile

in `tinycc/repo`:

`./configure`

## Create a compile_commands.json

in `tinycc/repo`:

`intercept-build make` or `bear make`

If your `compile_commands.json` enables optimizations(`-O2`, `-O3`, etc) you will need to remove them so that unsupported compiler_builtins are less likely to be generated and leave you in an uncompilable state.

Run `rm *.o` here to get rid of gcc generated staticlibs or else you may see `CRITICAL:root:error: some ELF objects were not compiled with clang:` in the next step

## Generate Rust Code

in `tinycc`:

`./translate.py` to translate all required c files into the `tinycc/repo/rust/src` directory.

## Run tinycc

Run `cargo run` to build and execute tinycc.

## Modified tests

`tests/tests2/70_floating_point_literals.c`, `tests/abitest.c`, and
`tests/tcctest.c` were modified due to use of `long double`s which do
not have a rust equivalent at this time (no `f128`).
