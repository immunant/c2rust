[![Build Status](https://travis-ci.org/immunant/c2rust.svg?branch=master)](https://travis-ci.org/immunant/c2rust)

# What is C2Rust?

C2Rust helps you migrate C99-compliant code to Rust. It provides:
- a C to Rust translator
- a Rust code refactoring tool
- tools to cross-check execution of the C code against the new Rust code

The translator (or transpiler), produces unsafe Rust code that closely mirrors the input C code. The primary goal of the translator is to produce code that is functionally identical to the input C code. Generating safe or idomatic Rust is *not* a goal for the translator. Rather, we think the best approach is to gradually rewrite the translated Rust code using dedicated refactoring tools. To this end, we are building a refactoring tool that rewrites unsafe auto-translated Rust into safer idioms (see [c2rust-refactor](c2rust-refactor/)).

Some refactoring will have to be done by hand which may introduce errors. We provide plugins for `clang` and `rustc` so you can compile and run two binaries and check that they behave identically (at the level of function calls). For details on cross-checking see the [cross-checks](cross-checks) directory and the cross checking [tutorial](docs/cross-check-tutorial.md).

Here's the big picture:

![C2Rust overview](docs/c2rust-overview.png "C2Rust overview")

To learn more, check out our [RustConf'18](https://www.youtube.com/watch?v=WEsR0Vv7jhg) talk on YouTube and try the C2Rust translator online at [www.c2rust.com](https://www.c2rust.com).

# Installation

## Prerequisites

C2Rust requires LLVM 6 or 7 and its corresponding libraries and clang compiler. Python 3.4 or later and CMake 3.4.3 or later are also required. These prerequisites may be installed with the following commands, depending on your platform:

- **Ubuntu 16.04, 18.04 & 18.10:**

        apt install build-essential llvm-6.0 clang-6.0 libclang-6.0-dev cmake libssl-dev

- **Arch Linux:**

        pacman -S base-devel llvm clang cmake openssl


- **OS X:** XCode command-line tools and recent LLVM (we recommend the Homebrew version) are required.

        xcode-select --install
        brew install llvm python3 cmake openssl


Finally, a rust installation with [Rustup](https://rustup.rs/) is required on all platforms.


## Building C2Rust

    $ cargo build --release

This builds the `c2rust` tool in the `target/release/` directory.

On OS X with Homebrew LLVM, you need to point the build system at the LLVM installation as follows:

    $ LLVM_CONFIG_PATH=/usr/local/opt/llvm/bin/llvm-config cargo build

If you have trouble with cargo build, the [developer docs](docs/README-developers.md#building-with-system-llvm-libraries) provide more details on the build system.

# Translating C to Rust

The translator needs access to the exact compiler commands used to build the C code. To provide this information, you will need a [standard](https://clang.llvm.org/docs/JSONCompilationDatabase.html) `compile_commands.json` file. Many build systems can automatically generate this file, as it is used by many other tools, but see [below](#generating-compile_commandsjson-files) for recommendations on how to generate this file for common build processes.

Once you have a `compile_commands.json` file describing the C build, translate the C code to Rust with the following command:

    $ ./scripts/transpile.py path/to/compile_commands.json

To generate a `Cargo.toml` template for a Rust library, add the `-e` option:

    $ ./scripts/transpile.py -e path/to/compile_commands.json

To generate a `Cargo.toml` template for a Rust binary, do this:

    $ ./scripts/transpile.py -m myprog path/to/compile_commands.json

Where `-m myprog` tells the transpiler to use the `main` method from `myprog.rs` as the entry point.

The translated Rust files will not depend directly on each other like
normal Rust modules. They will export and import functions through the C
API. These modules can be compiled together into a single static Rust
library.

There are several [known limitations](docs/known-limitations.md)
in this translator. The translator will attempt to skip function definitions that use unsupported features.

## Generating `compile_commands.json` files

The `compile_commands.json` file can be automatically created using
either `cmake`, `intercept-build`, or `bear`.

It may be a good idea to remove optimizations(`-OX`) from the compile commands
file, as there are optimization builtins which we do not support translating.

### ... with `cmake`

When creating the initial build directory with cmake specify
`-DCMAKE_EXPORT_COMPILE_COMMANDS=1`. This only works on projects
configured to be built by `cmake`. This works on Linux and MacOS.

    $ mkdir build
    $ cd build
    $ cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..

### ... with `intercept-build`

Intercept build is distributed with clang and recommended for makefile projects on macOS.

	$ intercept-build make
	$ intercept-build xcodebuild

### ... with `bear` (linux only)

When building on Linux, *Bear* is automatically built by the
`build_translator.py` script and installed into the `dependencies`
directory.

    $ ./configure CC=clang
    $ bear make

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
