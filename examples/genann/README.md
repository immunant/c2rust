# genann (Neural Network Library)

## Getting Started

If the repo submodule appears to be empty or out of date, you may need to run `git submodule update --init path/to/repo`.

## Required Manual Changes

This library requires no manual changes as part of translating to valid Rust code; however, it may require slight modification when attempting to link to example executables.

## Required Exporter Params

Pass the `-Wwrite-strings` flag to ensure global strings are exported correctly (required only for `example3` and `example4` but not the library or other examples).

## Required Importer Params

Pass the `--translate-entry` flag to the importer so that a valid rust main function is generated when translating the examples, but not the main library `genann.c`. The relooper is not strictly required.

## Linking

Once `genann.c` has been translated to to rust, you need create a static library like so: `rustc --crate-type=staticlib genann.rs` which should create a `libgenann.a` file.

Then pick `example1.c`, `example3.c`, or `example4.c` and translate it to rust (`example2.c` seems to never halt in both C and Rust but translates and executes just fine).

Lastly, you need to add `#[link(name = "genann")]` before the `extern "C" { ... }` block in your `exampleX.rs`. You can then build an executable like so: `rustc exampleX.rs -L path/to/staticlibdirectory/`.
