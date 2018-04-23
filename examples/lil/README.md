# lil (Little Interpreted Language)

## Getting Started

If the repo submodule appears to be empty or out of date, you may need to run `git submodule update --init path/to/repo`.

## Required Manual Changes

This library requires no manual changes as part of translating to valid Rust code; however, it may require slight modification when attempting to link to the executable.

## Required Exporter Params

No params are strictly required at this time.

## Required Importer Params

Pass the `--translate-entry` flag to the importer so that a valid rust main function is generated when translating `main.c`, but not the library `lil.c`. The relooper flag `--reloop-cfgs` is also required to generate certain portions of code.

## Linking

Once `lil.c` has been translated to to rust, you need create a static library like so: `rustc --crate-type=staticlib lil.rs` which will create a `liblil.a` file.

Lastly, you need to add `#[link(name = "lil")]` before the `extern "C" { ... }` block in your translated `main.rs`. You can then build an executable like so: `rustc main.rs -L path/to/staticlibdirectory/`.
