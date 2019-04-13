# C2Rust &emsp; [![Travis Build Status]][travis] [![Azure Build Status]][azure] [![Latest Version]][crates.io] ![Rustc Version]

[Travis Build Status]: https://api.travis-ci.org/immunant/c2rust.svg?branch=master
[travis]: https://travis-ci.org/immunant/c2rust
[Azure Build Status]: https://dev.azure.com/immunant/c2rust/_apis/build/status/immunant.c2rust?branchName=master
[azure]: https://dev.azure.com/immunant/c2rust/_build/latest?definitionId=1&branchName=master
[Latest Version]: https://img.shields.io/crates/v/c2rust.svg
[crates.io]: https://crates.io/crates/c2rust
[Docs]: https://docs.rs/c2rust/badge.svg
[docs.rs]: https://docs.rs/c2rust
[Rustc Version]: https://img.shields.io/badge/rustc-nightly--2019--04--08-lightgrey.svg

# Quickstart

    cargo +nightly-2019-04-08 install c2rust

# What is C2Rust?

C2Rust helps you migrate C99-compliant code to Rust. It provides:
- a C to Rust translator
- a Rust code refactoring tool
- tools to cross-check execution of the C code against the new Rust code

The translator (or transpiler), produces unsafe Rust code that closely mirrors the input C code. The primary goal of the translator is to produce code that is functionally identical to the input C code. Generating safe or idomatic Rust is *not* a goal for the translator. Rather, we think the best approach is to gradually rewrite the translated Rust code using dedicated refactoring tools. To this end, we are building a [refactoring tool](c2rust-refactor/) that rewrites unsafe auto-translated Rust into safer idioms.

Some refactoring will have to be done by hand which may introduce errors. We provide plugins for `clang` and `rustc` so you can compile and run two binaries and check that they behave identically (at the level of function calls). For details on cross-checking see the [cross-checks](cross-checks) directory and the cross checking [tutorial](docs/cross-check-tutorial.md).

Here's the big picture:

![C2Rust overview](docs/c2rust-overview.png "C2Rust overview")

To learn more, check out our [RustConf'18](https://www.youtube.com/watch?v=WEsR0Vv7jhg) talk on YouTube and try the C2Rust translator online at [www.c2rust.com](https://www.c2rust.com).

# Installation

## Prerequisites

C2Rust requires LLVM 6 or 7 and its corresponding libraries and clang compiler. Python 3.4 or later, CMake 3.4.3 or later, and openssl (1.0) are also required. These prerequisites may be installed with the following commands, depending on your platform:

- **Ubuntu 16.04, 18.04 & 18.10:**

        apt install build-essential llvm-6.0 clang-6.0 libclang-6.0-dev cmake libssl-dev pkg-config

- **Arch Linux:**

        pacman -S base-devel llvm clang cmake openssl


- **OS X:** XCode command-line tools and recent LLVM (we recommend the Homebrew version) are required.

        xcode-select --install
        brew install llvm python3 cmake openssl


Finally, a rust installation with [Rustup](https://rustup.rs/) is required on all platforms. You will also need to install `rustfmt`:

    rustup component add rustfmt


## Installing from crates.io

    cargo +nightly-2019-04-08 install c2rust

On OS X with Homebrew LLVM, you need to point the build system at the LLVM installation as follows:

    LLVM_CONFIG_PATH=/usr/local/opt/llvm/bin/llvm-config cargo build


If you have trouble with building and installing, or want to build from the latest master, the [developer docs](docs/README-developers.md#building-with-system-llvm-libraries) provide more details on the build system.

# Translating C to Rust

To translate C files specified in `compile_commands.json` (see below), run the `c2rust` tool with the `transpile` subcommand:

    c2rust transpile compile_commands.json

(The `c2rust refactor` tool is also available for refactoring Rust code, see [refactoring](c2rust-refactor/)).

The translator requires the exact compiler commands used to build the C code. To provide this information, you will need a [standard](https://clang.llvm.org/docs/JSONCompilationDatabase.html) `compile_commands.json` file. Many build systems can automatically generate this file, as it is used by many other tools, but see [below](#generating-compile_commandsjson-files) for recommendations on how to generate this file for common build processes.

Once you have a `compile_commands.json` file describing the C build, translate the C code to Rust with the following command:

    c2rust transpile path/to/compile_commands.json

To generate a `Cargo.toml` template for a Rust library, add the `-e` option:

    c2rust transpile --emit-build-files path/to/compile_commands.json

To generate a `Cargo.toml` template for a Rust binary, do this:

    c2rust transpile --main myprog path/to/compile_commands.json

Where `--main myprog` tells the transpiler to use the `main` method from `myprog.rs` as the entry point.

The translated Rust files will not depend directly on each other like
normal Rust modules. They will export and import functions through the C
API. These modules can be compiled together into a single static Rust
library or binary.

There are several [known limitations](docs/known-limitations.md) in this
translator. The translator will emit a warning and attempt to skip function
definitions that cannot be translated.

## Generating `compile_commands.json` files

The `compile_commands.json` file can be automatically created using
either `cmake`, `intercept-build`, or `bear`.

It may be a good idea to remove optimizations(`-OX`) from the compile commands
file, as there are optimization builtins which we do not support translating.

### ... with `cmake`

When creating the initial build directory with cmake specify
`-DCMAKE_EXPORT_COMPILE_COMMANDS=1`. This only works on projects
configured to be built by `cmake`. This works on Linux and MacOS.

    cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ...

### ... with `intercept-build`

intercept-build (part of the [scan-build
tool](https://github.com/rizsotto/scan-build)) is recommended for non-cmake
projects. intercept-build is bundled with clang under `tools/scan-build-py` but
a standalone version can be easily installed via PIP with:

    pip install scan-build

Usage:

    intercept-build <build command>

You can also use intercept-build to generate a compilation database for compiling a single C file, for example:

    intercept-build sh -c "cc program.c"

### ... with `bear` (linux only)

If you have [bear](https://github.com/rizsotto/Bear) installed, it can be used similarly to intercept-build:

    bear <build command>


# FAQ

> Are there release binaries? Can I install C2Rust with Cargo?

We plan to release a cargo package that you can `cargo install` soon(tm).

> I translated code on platform X but it didn't work correctly on platform Y

We run the C preprocessor before translation to Rust. This specializes the code to the host platform. For this reason, we do not support cross compiling translated code at the moment. 

> What platforms can C2Rust be run on?

The translator and refactoring tool support both macOS and Linux. Other features, such as cross checking the functionality between C and Rust code, are currently limited to Linux hosts. 


# Contact
To report issues with the translation, please use our Issue Tracker.

The development team can be reached by email at c2rust@immunant.com.

# Acknowledgements and Licensing

This material is available under the BSD-3 style license as found in the
[LICENSE](LICENSE) file.

The C2Rust translator is inspired by Jamey Sharp's [Corrode](https://github.com/jameysharp/corrode) translator. We rely on 
[Emscripten](https://github.com/kripken/emscripten)'s 
Relooper algorithm to translate arbitrary C control flows.

This material is based upon work supported by the United States Air Force and
DARPA under Contract No. FA8750-15-C-0124.  Any opinions, findings and
conclusions or recommendations  expressed in this material are those of the
author(s) and do not necessarily reflect the views of the United States Air
Force and DARPA.  Distribution Statement A, “Approved for Public Release,
Distribution Unlimited.”
