[![Docs]][docs.rs]

[Docs]: https://docs.rs/c2rust-transpile/badge.svg
[docs.rs]: https://docs.rs/c2rust-transpile

# C2Rust Transpiler

## Basic Usage

The transpiler module is invoked using the `transpile` sub-command of `c2rust`:

```sh
c2rust transpile [args] compile_commands.json [-- extra-clang-args]
```

The transpiler can also be given source files directly, which is ideal for projects that are trivially compiled, e.g.:

```sh
c2rust transpile [args] *.h *.c [-- extra-clang-args]
```

The following arguments control the basic transpiler behavior:

- `--emit-modules` - Emit each translated Rust file as a module 
  (the default is to make each file its own crate).
- `--fail-on-error` - Fail instead of warning if a source file cannot be fully translated.
- `--reduce-type-annotations` - Do not emit explicit type annotations when unnecessary.
- `-f <regex>`, `--filter <regex>` - Only translate files based on the regular expression used.

## Creating `cargo` build files

The transpiler can create skeleton `cargo` build files for the translated Rust sources, controlled by the following options:

- `-e`, `--emit-build-files` (implies `--emit-modules`) -
  Emit `cargo` build files to build the translated Rust code as a library.
  Build files are emitted in the directory specified by `--output-dir`, or if not specified,
  the directory containing `compile_commands.json`.  This will not overwrite existing files,
  so remove these build files before re-creating build files. 
- `-b <main_module>`, `--binary <main_module>` (implies `--emit-build-files`) -
  Emit `cargo` build files to build the translated Rust code as a binary.
  The `main` function must be found in the specified module (C source file) `<main_module>`. `<main_module>` should be the bare module name, not including the `.rs` extension.
  Build files are emitted in the directory specified by `--output-dir`, or if not specified,
  the directory containing `compile_commands.json`. This will not overwrite existing files,
  so remove this build file directory before re-creating build files. 

## For Developers

The `c2rust-transpile` library uses the `c2rust-ast-exporter` library to translate C
code to Rust. The `c2rust-ast-exporter` library links against the native clang compiler
front end to parse C code and exports the AST for use in the transpiler, which
is then implemented purely in Rust.
