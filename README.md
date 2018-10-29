# C2Rust

[![Build Status](https://travis-ci.org/immunant/c2rust.svg?branch=master)](https://travis-ci.org/immunant/c2rust)

## Why should you migrate your C code to Rust?

Rust is well known to be able to enforce many memory safety properties that C cannot. C2Rust's goal is to
automate much of the migration process from C to Rust, and to provide assistance where it cannot be automated.

A common complaint we hear goes something like this:

> I translated my C code, but the output is just as unsafe and not very idiomatic!

Although we do strive to improve translated code quality as much as possible, the goal of the translator
is ultimately to produce a 1-1 translation into Rust. Therefore, the resulting output will never be
completely idiomatic.

Since unsafe "C code in Rust" isn't the final goal, the C2Rust project isn't just a translator. There are two other major
components: a refactoring tool & cross checking plugin. These two tools can be used side by side to
incrementally idiomize and validate that your Rust code functions just like the original C, respectively.

In the end you should have safer, more idiomatic, Rust code. Our ultimate goal is to automate as much
of this process as possible, but refactoring is currently a manual process.

## Translation

The `ast-exporter` extracts from a C file the abstract syntax tree and type information produced by
Clang and serializes it into CBOR files. The `ast-importer` consumes these CBOR files and generates
Rust source code preserving the semantics (as understood under C99) of the initial C program.

The translated Rust files will not depend directly on each other like
normal Rust modules. They will export and import functions through the C
API. These modules can be compiled together into a single static Rust
library.

There are several [known limitations](docs/known-limitations.md)
in this translator. Some of these restrictions come from limitations of
Rust and others come from complexities of the features themselves. The
translator will attempt to skip function definitions that use
unsupported features.

### Setting up a build environment

There are three ways to build the C2Rust project:

#### Vagrant

In the provided vagrant environment. See the [vagrant README](vagrant/README.md)

#### Docker

In the provided docker environment. See the [docker README](docker/README.md)

#### Manually

The previous two options automatically install all pre-requisites during provisioning. With this option, prerequistics must be installed manually on a macOS or Linux system.

1) If you are on a Debian-based OS, you can run `scripts/provision_deb.sh` to do so. 

2) If you are on macOS, install the Xcode command-line tools (e.g., `xcode-select --install`) and [homebrew](https://brew.sh/) first. Then run `scripts/provision_mac.sh`.

3) If you prefer to install dependencies yourself, or are using a non Debian-based Linux OS, our dependencies are as follows:
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

### Building

These two projects have some large dependencies (namely parts of LLVM and Clang). If 
you've installed the necessary tools, the following should build the `ast-exporter`, 
`ast-importer`, and all of their dependencies, automatically pulling them in if 
necessary.

Building from scratch takes on the order of 30 minutes. The script has been tested on recent versions of macOS and Ubuntu.

    $ ./scripts/build_translator.py

To manually build the `ast-exporter`, check out [these build instructions][0]. To manually build the
`ast-importer`, check out [its README](ast-importer/README.md).

### Testing

Tests are found in the [`tests`](tests) folder. If both the `ast-exporter` and `ast-importer` are
built, you should be able to run the tests with:

    $ ./scripts/test_translator.py tests

This basically tests that the original C file and translated Rust file produce the same output when
compiled and run. More details about tests are in [this README](tests/README.md).

 [0]: docs/building-ast-exporter.md

### Using the translator

The C2Rust translation process relies on Clang to parse and type-check
input C files. For Clang to do this, it needs to know information that is
passed in via command-line flags. This information can be found in an
automatically generated `compile_commands.json`.

The `compile_commands.json` file can be automatically created using
either `cmake`, `intercept-build`, or `bear` (Linux only).

It may be a good idea to remove optimizations(`-OX`) from the compile commands
file, as there are optimization builtins which we do not support translating.

#### Generating `compile_commands.json` with `cmake`

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

#### Translating source files

The `transpile.py` script will automatically translate all of the C
source files mentioned in the previously generated
`compile_commands.json`.

    $ scripts/transpile.py ./compile_commands.json

## Refactoring

The refactoring tool, idiomize, has different passes to idiomize translated Rust code.
Some of the passes are run on idividual files and some on an entire Cargo build directory.
More detailed information on the tool can be found [here](rust-refactor).

## Cross Checking

Cross checking consists of plugins for both Rust and Clang. These plugins allow you to use
one of a couple different backends to compare runs of your C and Rust executables. This helps
you ensure that any idiomization of your Rust code still produces equivalent functionality
to the original C. More information on the tool can be found [here](cross-checks).

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
