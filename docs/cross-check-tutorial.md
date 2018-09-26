# Cross-checking Tutorial
## Introduction
The C2Rust transpiler aims to convert C code to semantically equivalent unsafe Rust code,
and later incremental refactoring passes gradually transform this code to Rust code.
However, the initial Rust translation might not be a perfect semantic match to the original C code,
and the refactoring passes may also change the code in ways that break semantics.
Cross-checking is an automated way to verify that the translated program behaves the same as the original C code.

The way cross-checking achieves this goal is by comparing the execution traces of all versions of the program 
(original C, unsafe and refactored Rust) and checking for any differences.
Our cross-checking implementation modifies the source code of the program at compile-time (separately during C and Rust compiler invocation) 
so that the programs output the traces at run-time, and then checks the traces against each other either online during execution 
(using the ReMon MVEE), or offline by comparing log files.
The C2Rust cross-checkers currently instrument function entry and exit points, function return values, 
and function call arguments (currently experimental and disabled by default, but can be enabled per argument, function or file).

## Building code with cross-checks
C2Rust contains one cross-checking implementation per language, in the form of a compiler plugin in both cases.
We provide a clang plugin for C code, and a rustc plugin for Rust code.

### Building C code
To build C programs with cross-checks enabled, first build the cross-checking plugin using `$C2RUST/scripts/build_cross_checks.py`, 
then run clang (or pass it to the build system) with the following options:
  * `-Xclang -load -Xclang $C2RUST/dependencies/clang-xcheck-plugin.$(uname -n)/plugin/CrossChecks.so` to load the plugin
  * `-Xclang -add-plugin -Xclang crosschecks` to activate the plugin
  * `-Xclang -plugin-arg-crosschecks -Xclang <...>` for every additional option to pass to the plugin
  * `-ffunction-sections` may be required to correctly deduplicate some linkonce functions inserted by the plugin
  
Note that every option passed to clang requires a `-Xclang` prefix before every actual option, 
so that the compiler driver passes it to its backend correctly.
We provide a `cc_wrapper.sh` script in the plugin source code directory that inserts these automatically,
as well as several project-specific scripts in directories under `examples/`.

Additionally, the following arguments should be passed to the linker:
  * The cross-checking runtime library from `$C2RUST/dependencies/clang-xcheck-plugin.$(uname -n)/runtime/libruntime.a`
  * A cross-checking backend library that provides the `rb_xcheck` function, 
    e.g., `libfakechecks` for offline logging or `libclevrbuf` for online MVEE-based checks

### Building Rust code
Building Rust code with cross-checks is simpler that C code, and only requires a few additions 
to `Cargo.toml` and the main Rust source file. Add the following to your `Cargo.toml` file 
(replacing `$C2RUST` to the actual path to this repository):
```TOML
[dependencies.cross-check-plugin]
path = "$C2RUST/cross-checks/rust-checks/rustc-plugin"

[dependencies.cross-check-derive]
path = "$C2RUST/cross-checks/rust-checks/derive-macros"

[dependencies.cross-check-runtime]
path = "$C2RUST/cross-checks/rust-checks/runtime"
features = ["libc-hash", "fixed-length-array-hash"]
```
and this preamble to your `lib.rs` or `main.rs`:
```Rust
#![feature(plugin, custom_attribute)]
#![cross_check(yes)]

#[macro_use] extern crate cross_check_derive;
#[macro_use] extern crate cross_check_runtime;
```

You may also add `#![plugin(cross_check_plugin(...))]` to pass additional arguments to the cross-checking plugin.

### Cross-check configuration
Cross-checks can be customized at a fine granularity using [cross-check configuration files or inline attributes](cross-check-config.md).

## Running cross-checked programs
### Offline mode
We currently provide a few offline cross-checking backend libraries:
  * `libfakechecks` outputs a list of the cross-checks linearly to either standard output or to a file 
  (specified using the `FAKECHECKS_OUTPUT_FILE` environment variable)
  * `fakechecks-zstd` library from `cross-checks/rust-checks/backends` (can also be used with the clang plugin) 
  outputs a binary encoding of the cross-checks that is compressed using zstd, and is much more space-efficient than 
  the text output of `libfakechecks`. The compressed output files can be converted to text using the `xcheck-printer` tool.
  
Before running the C and Rust binaries, you may need to load in one of these libraries using `LD_PRELOAD` if you 
haven't linked against it and passed in its path using `-rpath` (this is fairly easy to do for a C build, but 
more complicated when using Cargo for Rust code), like this:
```Bash
  $ env LD_PRELOAD=$C2RUST/cross-checks/libfakechecks/libfakechecks.so ./a.out
```

Running each program version with cross-checks enabled will print a list of cross-check results either to standard output
or to the specified file. A simple `diff` or `cmp` command will show differences in traces, if any.

### Online (MVEE) mode
TODO: describe MVEE setup

## Troubleshooting
TODO: various issues and fixes
