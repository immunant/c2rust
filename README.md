[![Travis Build Status]][travis] [![Azure Build Status]][azure] [![Latest Version]][crates.io] [![Rustc Version]](#)

[Travis Build Status]: https://api.travis-ci.org/immunant/c2rust.svg?branch=master
[travis]: https://travis-ci.org/immunant/c2rust
[Azure Build Status]: https://dev.azure.com/immunant/c2rust/_apis/build/status/immunant.c2rust?branchName=master
[azure]: https://dev.azure.com/immunant/c2rust/_build/latest?definitionId=1&branchName=master
[Latest Version]: https://img.shields.io/crates/v/c2rust.svg
[crates.io]: https://crates.io/crates/c2rust
[Rustc Version]: https://img.shields.io/badge/rustc-nightly--2019--10--04-lightgrey.svg "Rustc nightly-2019-10-04"

C2Rust helps you migrate C99-compliant code to Rust. The [translator](c2rust-transpile) (or transpiler) produces unsafe Rust code that closely mirrors the input C code. The primary goal of the translator is to preserve functionality; test suites should continue to pass after translation. Generating safe and idiomatic Rust code from C ultimately requires manual effort. However, we are building a scriptable [refactoring tool](c2rust-refactor) that reduces the tedium of doing so. You can also [cross-check](cross-checks) the translated code against the original ([tutorial](docs/cross-check-tutorial.md)).

Here's the big picture:

![C2Rust overview](docs/c2rust-overview.png "C2Rust overview")

To learn more, check out our [RustConf'18](https://www.youtube.com/watch?v=WEsR0Vv7jhg) talk on YouTube and try the C2Rust translator online at [www.c2rust.com](https://www.c2rust.com).

## Documentation

To learn more about using and developing C2Rust, check out the [manual](https://c2rust.com/manual/). The manual is still a work-in-progress, so if you can't find something please let us know.

## Installation

### Prerequisites

C2Rust requires LLVM 6, 7, or 8 with its corresponding clang compiler and libraries. Python 3.4 or later, CMake 3.4.3 or later, and openssl (1.0) are also required. These prerequisites may be installed with the following commands, depending on your platform:

- **Ubuntu 16.04, 18.04 & 18.10:**

        apt install build-essential llvm-6.0 clang-6.0 libclang-6.0-dev cmake libssl-dev pkg-config

- **Arch Linux:**

        pacman -S base-devel llvm clang cmake openssl
        
- **NixOS / nix:**

        nix-shell

- **OS X:** XCode command-line tools and recent LLVM (we recommend the Homebrew version) are required.

        xcode-select --install
        brew install llvm python3 cmake openssl


Finally, installing the correct nightly Rust compiler with [Rustup](https://rustup.rs/) is required on all platforms. You will also need to add `rustfmt`:

    rustup install nightly-2019-10-04
    rustup component add --toolchain nightly-2019-10-04 rustfmt


### Installing from crates.io

    cargo +nightly-2019-10-04 install c2rust

On OS X with Homebrew LLVM, you need to point the build system at the LLVM installation as follows:

    LLVM_CONFIG_PATH=/usr/local/opt/llvm/bin/llvm-config cargo +nightly-2019-10-04 install c2rust

On Linux with Linuxbrew LLVM, you need to point the build system at the LLVM installation as follows:

    LLVM_CONFIG_PATH=/home/linuxbrew/.linuxbrew/opt/llvm/bin/llvm-config cargo +nightly-2019-10-04 install c2rust    

Note: adjust `LLVM_CONFIG_PATH` accordingly if Linuxbrew was installed to your home directory.

On Gentoo, you need to point the build system to the location of `libclang.so` 
  and `llvm-config` as follows:

    LLVM_CONFIG_PATH=/path/to/llvm-config LIBCLANG_PATH=/path/to/libclang.so cargo +nightly-2019-10-04 install c2rust 


If you have trouble with building and installing, or want to build from the latest master, the [developer docs](docs/README-developers.md#building-with-system-llvm-libraries) provide more details on the build system.

### Installing from Git

If you'd like to check our recently developed features or you urgently require a bugfixed version of c2rust
you can install it directly from Git:

    cargo +nightly-2019-10-04 install --git https://github.com/immunant/c2rust.git c2rust
   
Please note that the master branch is under constant development and you may expirience issues or crashes.

You should also set `LLVM_CONFIG_PATH` accordingly if required as described above.

## Translating C to Rust

To translate C files specified in `compile_commands.json` (see below), run the `c2rust` tool with the `transpile` subcommand:

    c2rust transpile compile_commands.json

(The `c2rust refactor` tool is also available for refactoring Rust code, see [refactoring](c2rust-refactor/)).

The translator requires the exact compiler commands used to build the C code. This information is provided via a compilation database file named `compile_commands.json`. (Read more about compilation databases [here](https://clang.llvm.org/docs/JSONCompilationDatabase.html) and [here](https://sarcasm.github.io/notes/dev/compilation-database.html)). Many build systems can automatically generate this file; we show a few examples [below](#generating-compile_commandsjson-files).

Once you have a `compile_commands.json` file describing the C build, translate the C code to Rust with the following command:

    c2rust transpile path/to/compile_commands.json

To generate a `Cargo.toml` template for a Rust library, add the `-e` option:

    c2rust transpile --emit-build-files path/to/compile_commands.json

To generate a `Cargo.toml` template for a Rust binary, do this:

    c2rust transpile --binary myprog path/to/compile_commands.json

Where `--binary myprog` tells the transpiler to use the `main` method from `myprog.rs` as the entry point for a binary.

The translated Rust files will not depend directly on each other like
normal Rust modules. They will export and import functions through the C
API. These modules can be compiled together into a single static Rust
library or binary.

There are several [known limitations](docs/known-limitations.md) in this
translator. The translator will emit a warning and attempt to skip function
definitions that cannot be translated.

### Generating `compile_commands.json` files

The `compile_commands.json` file can be automatically created using
either `cmake`, `intercept-build`, or `bear`.

It may be a good idea to remove optimizations(`-OX`) from the compile commands
file, as there are optimization builtins which we do not support translating.

#### ... with `cmake`

When creating the initial build directory with cmake specify
`-DCMAKE_EXPORT_COMPILE_COMMANDS=1`. This only works on projects
configured to be built by `cmake`. This works on Linux and MacOS.

    cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ...

#### ... with `intercept-build`

intercept-build (part of the [scan-build
tool](https://github.com/rizsotto/scan-build)) is recommended for non-cmake
projects. intercept-build is bundled with clang under `tools/scan-build-py` but
a standalone version can be easily installed via PIP with:

    pip install scan-build

Usage:

    intercept-build <build command>

You can also use intercept-build to generate a compilation database for compiling a single C file, for example:

    intercept-build sh -c "cc program.c"

#### ... with `bear` (linux only)

If you have [bear](https://github.com/rizsotto/Bear) installed, it can be used similarly to intercept-build:

    bear <build command>

## Contact
To report issues with translation or refactoring, please use our [Issue Tracker](https://github.com/immunant/c2rust/issues).

To reach the development team, join our [discord channel](https://discord.gg/ANSrTuu) or email us at [c2rust@immunant.com](mailto:c2rust@immunant.com).

## FAQ

> I translated code on platform X but it didn't work correctly on platform Y

We run the C preprocessor before translation to Rust. This specializes the code to the host platform. For this reason, we do not support cross compiling translated code at the moment. 

> What platforms can C2Rust be run on?

The translator and refactoring tool support both macOS and Linux. Other features, such as cross checking the functionality between C and Rust code, are currently limited to Linux hosts. 

## Acknowledgements and Licensing

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
