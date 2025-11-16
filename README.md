# C2Rust

[![ci GitHub Actions Status]][github] [![c2rust-testsuite GitHub Actions Status]][github] [![Latest Version]][crates.io] [![Rustc Version]](#)

[ci GitHub Actions Status]: https://github.com/immunant/c2rust/workflows/ci/badge.svg
[c2rust-testsuite GitHub Actions Status]: https://github.com/immunant/c2rust/workflows/c2rust-testsuite/badge.svg
[github]: https://github.com/immunant/c2rust/actions

[Latest Version]: https://img.shields.io/crates/v/c2rust.svg
[crates.io]: https://crates.io/crates/c2rust
[rustc Version]: https://img.shields.io/badge/rustc-nightly--2022--08--08-lightgrey.svg "rustc nightly-2022-08-08"

<!-- ANCHOR: intro -->

## Intro

C2Rust helps you migrate C99-compliant code to Rust.
The translator (or transpiler), [`c2rust transpile`](./c2rust-transpile/),
produces unsafe Rust code that closely mirrors the input C code.
The primary goal of the translator is to preserve functionality;
test suites should continue to pass after translation.

Generating safe and idiomatic Rust code from C ultimately requires manual effort.
We are currently working on analysis to automate some of the effort
required to lift unsafe Rust into safe Rust types.
However, we are building a [refactoring tool](c2rust-refactor) that reduces the tedium of doing so.
This work is still in the early stages; please get in touch if you're interested!

Here's the big picture:

![C2Rust overview](docs/c2rust-overview.png "C2Rust overview")

To learn more, check out our [RustConf'18](https://www.youtube.com/watch?v=WEsR0Vv7jhg) talk on YouTube
and try the C2Rust translator online using the [Compiler Explorer](https://godbolt.org/z/GdEzYWGq4).
This uses the current `master` branch, updated every night.

<!-- ANCHOR_END: intro -->

## Documentation

To learn more about using and developing C2Rust, check out the [manual](https://c2rust.com/manual/).
The manual is still a work-in-progress, so if you can't find something please let us know.
[c2rust.com/manual/](https://c2rust.com/manual/) also has not been updated since ~2019,
so refer to the in-tree [./manual/](./manual/) for more up-to-date instructions.

<!-- ANCHOR: installation -->

## Installation

### Prerequisites

C2Rust requires LLVM 7 or later with its corresponding clang compiler and libraries.
Python (through `uv`), CMake 3.5 or later and openssl (1.0) are also required.
These prerequisites may be installed with the following commands, depending on your platform:

Python:

```sh
curl -LsSf https://astral.sh/uv/install.sh | sh
uv venv
uv pip install -r scripts/requirements.txt
```

- **Ubuntu 18.04, Debian 10, and later:**

    ```sh
    apt install build-essential llvm clang libclang-dev cmake libssl-dev pkg-config git
    ```

Depending on the LLVM distribution, the `llvm-dev` package may also be required.
For example, the official LLVM packages from [apt.llvm.org](https://apt.llvm.org/) require `llvm-dev` to be installed.

- **Arch Linux:**

    ```sh
    pacman -S base-devel llvm clang cmake openssl
    ```

- **NixOS / nix:**

    ```sh
    nix-shell
    ```

- **macOS:** Xcode command-line tools and recent LLVM (we recommend the Homebrew version) are required.

    ```sh
    xcode-select --install
    brew install llvm cmake openssl
    ```

The C2Rust transpiler now builds using a stable Rust compiler.
If you are developing other features,
you may need to install the correct nightly compiler version.

### Installing from crates.io

```sh
cargo install --locked c2rust
```

You can also set the LLVM version explicitly if you have multiple installed,
like this, for example:

```sh
LLVM_CONFIG_PATH=llvm-config-14 cargo install --locked c2rust
```

If you're using LLVM from Homebrew (either on Apple Silicon, Intel Macs, or Linuxbrew),
you can run:

```sh
LLVM_CONFIG_PATH="$(brew --prefix)/opt/llvm/bin/llvm-config" cargo install --locked c2rust
```

or for a specific LLVM version,

```sh
LLVM_CONFIG_PATH="$(brew --prefix)/opt/llvm@21/bin/llvm-config" cargo install --locked c2rust
```

On Gentoo, you need to point the build system to
the location of `libclang.so` and `llvm-config` as follows:

```sh
LLVM_CONFIG_PATH=/path/to/llvm-config LIBCLANG_PATH=/path/to/libclang.so cargo install --locked c2rust
```

If you have trouble with building and installing, or want to build from the latest master,
the [developer docs](docs/README-developers.md#building-with-system-llvm-libraries)
provide more details on the build system.

### Installing from Git

If you'd like to check our recently developed features or you urgently require a bugfixed version of `c2rust`,
you can install it directly from Git:

```sh
cargo install --locked --git https://github.com/immunant/c2rust.git c2rust
```

Please note that the master branch is under constant development and you may experience issues or crashes.

You should also set `LLVM_CONFIG_PATH` accordingly if required as described above.

### Nightly Tools

`c2rust` and `c2rust-transpile` are installed by default and can be built on `stable` `rustc`.
The other tools, such as `c2rust-refactor`, use `rustc` internal APIs, however,
and are thus pinned to a specific `rustc` `nightly` version: `nightly-2022-08-08`.
These are also not published to `crates.io`.
To install these, these can be installed with `cargo` with the pinned nightly.  For example,

```sh
cargo +nightly-2022-08-08 install --locked --git https://github.com/immunant/c2rust.git c2rust-refactor
```

However, we recommend installing them from a full checkout,
as this will resolve the pinned nightly automatically:

```sh
git clone https://github.com/immunant/c2rust.git
cd c2rust
cargo build --release
```

These tools, like `c2rust-refactor`, can then also be invoked through `c2rust`
as `c2rust refactor`, assuming they are installed in the same directory.

<!-- ANCHOR_END: installation -->
<!-- ANCHOR: translating-c-to-rust -->

## Translating C to Rust

To translate C files specified in `compile_commands.json` (see below),
run the `c2rust` tool with the `transpile` subcommand:

```sh
c2rust transpile compile_commands.json
```

`c2rust` also supports a trivial transpile of source files, e.g.:

```sh
c2rust transpile project/*.c project/*.h
```

For non-trivial projects, the translator requires the exact compiler commands used to build the C code.
This information is provided via a [compilation database](https://clang.llvm.org/docs/JSONCompilationDatabase.html)
file named `compile_commands.json` (note that it must be named exactly `compile_commands.json`;
otherwise `libclangTooling` can have (silent) trouble resolving it correctly).
(Read more about [compilation databases here](https://sarcasm.github.io/notes/dev/compilation-database.html)).
Many build systems can automatically generate this file;
we show [a few examples below](#generating-compile_commandsjson-files).

Once you have a `compile_commands.json` file describing the C build,
translate the C code to Rust with the following command:

```sh
c2rust transpile path/to/compile_commands.json
```

To generate a `Cargo.toml` template for a Rust library, add the `--emit-build-files` option:

```sh
c2rust transpile --emit-build-files path/to/compile_commands.json
```

To generate a `Cargo.toml` template for a Rust binary, do this:

```sh
c2rust transpile --binary myprog path/to/compile_commands.json
```

Where `--binary myprog` tells the transpiler to use
the `main` function from `myprog.rs` as the entry point for a binary.
This can be repeated multiple times for multiple binaries.

The translated Rust files will not depend directly on each other like
normal Rust modules.
They will export and import functions through the C API.
These modules can be compiled together into a single static Rust library or binary.

You can run with `--reorganize-definitions` (which invokes `c2rust-refactor`),
which should deduplicate definitions and directly import them
with `use`s instead of through the C API.

The refactorer can also be run on its own to run other refactoring passes:

```sh
c2rust refactor --cargo $transform
```

There are several [known limitations](./docs/known-limitations.md) in this
translator.
The translator will emit a warning and attempt to skip function
definitions that cannot be translated.

### Generating `compile_commands.json` Files

The `compile_commands.json` file can be automatically created
using either `cmake`, `meson`, `bear`, `intercept-build`, or `compiledb`.

It may be a good idea to remove optimizations (`-OX`) from the compilation database,
as there are optimization builtins which we do not support translating.

#### ... with `cmake`

When creating the initial build directory with `cmake`,
specify `-DCMAKE_EXPORT_COMPILE_COMMANDS=1`.
This only works on projects configured to be built by `cmake`.
This works on Linux and MacOS.

```sh
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ...
```

#### ... with `meson`

When creating the initial build directory with `meson`,
it will automatically generate a `compile_commands.json`
file inside of `<build_dir>`.

```sh
meson setup <build_dir>
```

#### ... with `bear`

[`bear`](https://github.com/rizsotto/Bear) is recommended for projects whose build systems
don't generate `compile_commands.json` automatically
(`make`, for example, unlike `cmake` or `meson`). It can also be useful
for `cmake` and `meson` to generate a subset of the full `compile_commands.json`,
as it records all compilations that a subcommand does.

It can be installed with

```sh
apt install bear
```

or

```sh
brew install bear
```

Usage:

```sh
bear -- <build command>
```

`<build command>` can be `make`, `make`/`cmake` for a single target, or a single `cc` compilation:

```sh
bear -- make
bear -- cmake --build . --target $target
bear -- cc -c program.c
```

Note that since it detects compilations,
if compilations are cached (by `make` for example),
you'll need a clean build first (e.g. `make clean`).

#### ... with `intercept-build`

`intercept-build` (part of the [scan-build](https://github.com/rizsotto/scan-build))
is very similar, but not always as up-to-date and comprehensive as `bear`.
`intercept-build` is bundled with `clang` under `tools/scan-build-py`,
but a standalone version can be easily installed via `pip` with:

```sh
uv tool install scan-build
```

#### ... with `compiledb`

The `compiledb` package can also be used for `make` projects if the other tools don't work.
Unlike the others, it doesn't require a clean build/`make clean`.
Install via `pip` with:

```sh
uv tool install compiledb
```

Usage:

```sh
# After running
./autogen.sh && ./configure # etc.
# Run
compiledb make
```

<!-- ANCHOR_END: translating-c-to-rust -->

## Contact

To report issues with translation or refactoring,
please use our [Issue Tracker](https://github.com/immunant/c2rust/issues).

To reach the development team, join our [discord channel](https://discord.gg/ANSrTuu)
or email us at [c2rust@immunant.com](mailto:c2rust@immunant.com).

## FAQ

> I translated code on platform X, but it didn't work correctly on platform Y.

We run the C preprocessor before translation to Rust.
This specializes the code to the target platform (usually the host platform).
We do, however, support cross-architecture transpilation with a different sysroot
(cross-OS transpilation is more difficult because
it can be difficult to get a sysroot for the target OS).
For example, on an `aarch64-linux-gnu` host, to cross-transpile to `x86_64-linux-gnu`,
you can run

```sh
sudo apt install gcc-x86-64-linux-gnu # install cross-compiler, which comes with a sysroot
c2rust transpile ${existing_args[@]} -- --target=x86_64-linux-gnu --sysroot=/usr/x86_64-linux-gnu
```

These extra args are passed to the `libclangTooling` that `c2rust-transpile` uses.
You sometimes also need to pass extra headers, as occasionally headers are installed globally
in the default sysroot and won't be found in the cross-compiling sysroot.

> What platforms can C2Rust be run on?

The translator and refactoring tool support both macOS and Linux.

## Uses of `c2rust transpile`

This is a list of all significant uses of `c2rust transpile` that we know of:

| Rust | C | By | Safety | Description |
| - | -- | - | - | - |
| [`rav1d`](https://github.com/memorysafety/rav1d/) | [`dav1d`](https://code.videolan.org/videolan/dav1d) | @memorysafety, @immunant | fully safe | AV1 decoder |
| [`rexpat`](https://github.com/immunant/rexpat) | [`libexpat`](https://github.com/libexpat/libexpat) | @immunant | safety unfinished | streaming XML parser |
| [`unsafe-libyaml`](https://github.com/dtolnay/unsafe-libyaml) | [`libyaml`](https://github.com/yaml/libyaml) | @dtolnay | minor cleanup, fully unsafe | YAML parser and writer used by [`serde_yaml`](https://github.com/dtolnay/serde-yaml)
| [`libyaml-safer`](https://github.com/simonask/libyaml-safer) | [`libyaml`](https://github.com/yaml/libyaml) | @simonask | fully safe | safe fork of [`unsafe-libyaml`](https://github.com/dtolnay/unsafe-libyaml) |
| [`libbzip2-rs`](https://github.com/trifectatechfoundation/libbzip2-rs) | [`bzip2`](https://gitlab.com/bzip2/bzip2) | @trifectatechfoundation | fully safe | file compression |
| [`tsuki`](https://github.com/ultimaweapon/tsuki) | [`lua`](https://www.lua.org/source/5.4/) | @ultimaweapon | fully safe | Lua interpreter |
| [`spiro.rlib`](https://github.com/MFEK/spiro.rlib) | [`spiro`](https://github.com/raphlinus/spiro) | @ctrlcctrlv | fully safe | spline interpolation |
| [`sapp-kms`](https://crates.io/crates/sapp-kms) | [`sokol`](https://github.com/floooh/sokol) | @not-fl3 | cleaned up, still unsafe | application rendering library |

If any other project successfully uses `c2rust`, feel free to add your ported project here.

## Acknowledgements and Licensing

This material is available under the BSD-3 style license as found in the
[LICENSE](./LICENSE) file.

The C2Rust translator is inspired by Jamey Sharp's [Corrode](https://github.com/jameysharp/corrode) translator.
We rely on [Emscripten](https://github.com/kripken/emscripten)'s
Relooper algorithm to translate arbitrary C control flows.
Many individuals have contributed bug fixes and improvements to C2Rust; thank you so much!

This material is based upon work supported by the United States Air Force and
DARPA under Contracts No. FA8750-15-C-0124, HR0011-22-C-0020, and HR00112590133.
Any opinions, findings and conclusions or recommendations expressed in this
material are those of the author(s) and do not necessarily reflect the views
of the United States Air Force or DARPA.

Distribution Statement A, "Approved for Public Release, Distribution Unlimited."
