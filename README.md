[![Build Status](https://travis-ci.org/immunant/c2rust.svg?branch=master)](https://travis-ci.org/immunant/c2rust)

# What is C2Rust?

C2Rust helps you migrate C99 compliant codebases into Rust. It provides:
- a C to Rust translator, 
- a Rust code refactoring tool, and
- a way to cross-check the C code against the new Rust code.

The translator (or transpiler), produces unsafe Rust code that closely mirrors the input C code. The primary goal of the translator is to produce code that is functionally identical to the input C code. Generating safe or idomatic Rust is *not* a goal for the translator. Rather, we think the best approach is to gradually clean up the translator using supplementary tools. To that end, we are building a refactoring tool (`idiomize`) - see the `rust-refactor` directory for details.
Some refactoring will have to be done by hand which may introduce errors. We provide plugins for `clang` and `rustc` so that you can compile and run two binaries and check that they behave the same (at the level of function calls).
See details in the `cross-checks` directory and in the cross checking [tutorial](docs/cross-check-tutorial.md).

# Setting up a development environment

There are three ways to build the C2Rust project:

- Using **Vagrant**. See our [vagrant README](vagrant/README.md).
- Using **Docker**. See our [docker README](docker/README.md).
- **Manually**, as explained below.

The previous two options automatically install all prerequisites during provisioning. You can also provision a macOS or Linux system manually.

* If you are on a Debian-based OS, you can run `scripts/provision_deb.sh` to do so. 

* If you are on macOS, install the Xcode command-line tools (e.g., `xcode-select --install`) and [homebrew](https://brew.sh/) first. Then run `scripts/provision_mac.sh`.

* If you prefer to install dependencies yourself, or are using a non Debian-based Linux OS, our dependencies are as follows:
    - cmake >= 3.9.1
    - dirmngr
    - curl
    - git
    - gnupg2
    - gperf
    - htop
    - ninja
    - unzip
    - clang 5.0+
    - intercept-build or bear - see section on `compile_commands.json` generation
    - python-dev
    - python 3.6+
    - [python dependencies](scripts/requirements.txt)
    - rustc [version](rust-toolchain)
    - rustfmt-preview component for the above rustc version

# Building

Building from scratch takes a little while. The script has been tested on recent versions of macOS and Ubuntu.

    $ ./scripts/build_translator.py

To use the cross checking functionality, add the following option.

    $ ./scripts/build_translator.py --with-clang

# Testing (Optional)

Tests are found in the [`tests`](tests) folder. If you build the translator successfully, you should be able to run the tests with:

    $ ./scripts/test_translator.py tests

This basically tests that the original C file and translated Rust file produce the same output when compiled and run. More details about tests are in [this README](tests/README.md).

 [0]: docs/building-ast-exporter.md

# Translating C to Rust

Once everything is set up, simply run:

    $ ./scripts/transpile.py path/to/compile_commands.json

<!-- TODO: shorten explanation for compile_commands.json -->
<!-- TODO: explain transpiler options (cargo.toml, binary, library, etc.) -->

The translated Rust files will not depend directly on each other like
normal Rust modules. They will export and import functions through the C
API. These modules can be compiled together into a single static Rust
library.

There are several [known limitations](docs/known-limitations.md)
in this translator. The translator will attempt to skip function definitions that use unsupported features.

The C2Rust translation process relies on Clang to parse and type-check
input C files. For Clang to do this, it needs to know information that is
passed in via command-line flags. This information can be found in an
automatically generated `compile_commands.json`.

The `compile_commands.json` file can be automatically created using
either `cmake`, `intercept-build`, or `bear` (Linux only).

It may be a good idea to remove optimizations(`-OX`) from the compile commands
file, as there are optimization builtins which we do not support translating.

## Generating `compile_commands.json` with `cmake`

When creating the initial build directory with cmake specify
`-DCMAKE_EXPORT_COMPILE_COMMANDS=1`. This only works on projects
configured to be built by cmake. This works on Linux and MacOS.

    $ mkdir build
    $ cd build
    $ cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..

#### Generating `compile_commands.json` with `intercept-build`

Intercept build is distributed with clang and recommended for makefile projects on macOS.

	$ intercept-build make
	$ intercept-build xcodebuild

#### Generating `compile_commands.json` with `bear`

When building on Linux, *Bear* is automatically built by the
`build_translator.py` script and installed into the `dependencies`
directory.

    $ ./configure CC=clang
    $ bear make



## Cross Checking

Cross checking consists of plugins for both Rust and Clang. These plugins allow you to use
one of a couple different backends to compare runs of your C and Rust executables. This helps
you ensure that any idiomization of your Rust code still produces equivalent functionality
to the original C. 

## FAQ

> Are there release binaries? Can I install C2Rust with Cargo?

We are currently looking into combining the `ast-extractor` and `ast-importer` translator
components so that release binaries and/or a `cargo install c2rust` installation might be possible.

> I translated code on platform X but it didn't work correctly on platform Y

We do not support cross compiling raw translated code. However, in the future, it may be possible
for the refactoring tool to ease some of these pains.

> What platforms can C2Rust be run on?

The translator and refactoring tool support both macOS and Linux. Other features, such as cross checking the functionality between C and Rust code, are currently limited to Linux hosts. 

## Acknowledgements and Licensing

This material is available under the BSD-3 style license as found in the
`LICENSE` file.

The C2Rust translator is inspired by Jamey Sharp's [Corrode](https://github.com/jameysharp/corrode) translator. We rely on 
[Emscripten](https://github.com/kripken/emscripten)'s 
Relooper algorithm to translate arbitrary C control flows.

This material is based upon work supported by the United States Air Force and
DARPA under Contract No. FA8750-15-C-0124.  Any opinions, findings and
conclusions or recommendations  expressed in this material are those of the
author(s) and do not necessarily reflect the views of the United States Air
Force and DARPA.  Distribution Statement A, “Approved for Public Release,
Distribution Unlimited.”
