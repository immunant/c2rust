# C2Rust Transpiler

## Basic Usage

The transpiler module is invoked using the `transpile` sub-command of `c2rust`:

    c2rust transpile [args] compile_commands.json [-- extra-clang-args]

The following arguments control the basic transpiler behavior:

- `--emit-modules` - Emit each translated Rust file as a module (the default is
  to make each file its own crate).
- `--fail-on-error` - Fail instead of warning if a source file cannot be fully
  translated.
- `--reduce-type-annotations` - Do not emit explicit type annotations when
  unnecessary.
- `--translate-asm` - Translate C inline assembly into corresponding Rust inline
  assembly. The translated assembly is unlikely to work as-is due to differences
  between GCC and LLVM (used in Rust) inline assembly styles, but it can provide
  a starting point for manual translation.

## Creating cargo build files

The transpiler can create skeleton cargo build files for the translated Rust sources, controlled by the following options:

- `-e`, `--emit-build-files` - Emit cargo build files to build the translated
  Rust code as a library. Build files are emitted into the `c2rust-build` in the
  current directory. This will not overwrite existing files, so remove this
  build file directory before re-creating build files. (implies `--emit-modules`)
- `-m <main_module>`, `--main <main_module>` - Emit cargo build files to build
  the translated Rust code as a binary. The main function must be found in the
  specified module (C source file) `<main_module>`. `<main_module>` should be
  the bare module name, not including the `.rs` extension. Build files are
  emitted into the `c2rust-build` in the current directory. This will not
  overwrite existing files, so remove this build file directory before
  re-creating build files. (implies `--emit-build-files`)

## Cross-check instrumentation

The transpiler can instrument the transpiled Rust code for
[cross-checking](../cross-checks/). The following options control this
instrumentation:

- `-x`, `--cross-checks` - Add macros and build files for cross-checking.
- `--use-fakechecks` - Link against the `fakechecks` library for cross-checking
  instead of using the default online checks.
- `-X <config>`, `--cross-check-config <config>` - Use the given config file as
  the cross-checking config.

## For Developers

The c2rust-transpile library uses the c2rust-ast-exporter library to translate C
code to Rust. The ast-exporter library links against the native clang compiler
front end to parse C code and exports the AST for use in the transpiler, which
is then implemented purely in Rust.
