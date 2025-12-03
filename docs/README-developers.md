# Setting up a development environment

- If you are on a Debian-based OS, you can run `./scripts/provision_deb.sh` to do so.

- If you are on macOS, install the Xcode command-line tools
(e.g., `xcode-select --install`) and [homebrew](https://brew.sh/) first.
Then run `./scripts/provision_mac.sh`.

- If you prefer to install dependencies yourself, or are using a non Debian-based Linux OS, our dependencies are as follows:
  - `cmake` >= 3.9.1
  - `dirmngr`
  - `curl`
  - `git`
  - `gnupg2`
  - `gperf`
  - `ninja`
  - `unzip`
  - `clang` >= 7
  - `intercept-build` or `bear` ([see why here](../README.md#generating-compile_commandsjson-files))
  - `uv`
  - [python dependencies](../scripts/requirements.txt)
  - `rustc` [version](../rust-toolchain.toml)
  - `rustfmt` component for the above `rustc` version
  - `libssl` (development library, dependency of the refactoring tool)

## Building with system LLVM libraries

The quickest way to build the C2Rust transpiler
is with LLVM and clang system libraries (LLVM/clang >= 7 are currently supported).
If you have `libLLVM.so` and the `libclang` libraries (`libclangAST.a`, `libclangTooling.a`, etc. or their shared variants) installed,
you can build the transpiler with:

```sh
cd c2rust-transpile
cargo build
```

You can customize the location where the build system will look for LLVM using the following environment variables at compile time:

- `LLVM_CONFIG_PATH`: path (or filename if in `$PATH`) to the `llvm-config` tool of the LLVM installation
- `LLVM_LIB_DIR`: path to the `lib` directory of the LLVM installation (not necessary if you use `LLVM_CONFIG_PATH`)
- `LLVM_SYSTEM_LIBS`: additional system libraries LLVM needs to link against (e.g. `-lz -lrt -ldl`). Not necessary with `llvm-config`.
- `CLANG_PATH`: path to a clang that is the same version as your `libclang.so`.
  If this is necessary, the build system will return an error message explaining that.

C2Rust (indirectly) uses the [`clang-sys`](https://crates.io/crates/clang-sys) crate,
which can be configured with its own environment variables.

## Building dependencies from source

To develop on components that interact with LLVM,
we recommend building against a local copy of LLVM.
This will ensure that you have debug symbols and IDE integration for both LLVM and C2Rust.
However, building C2Rust from source with LLVM takes a while.
For a shorter build that links against prebuilt LLVM and clang system libraries,
you should be able to `cargo build` in the [`c2rust-transpile` directory](../c2rust-transpile/)
(see the general [README](../README.md)).

The following from-LLVM-source full build script
has been tested on recent versions of macOS and Ubuntu:

```sh
./scripts/build_translator.py
```

This downloads and builds LLVM under a new top-level folder named `build`.
Use the `C2RUST_BUILD_SUFFIX` variable to do multiple side-by-side builds
against a local copy of LLVM like this:

```sh
C2RUST_BUILD_SUFFIX=.debug ./scripts/build_translator.py --debug
```

*Note*: Set `C2RUST_BUILD_SUFFIX` if building inside and outside of the provided Docker environments from a single C2Rust checkout.

## Testing (Optional)

Tests are found in the [`tests`](../tests/) folder.
If you build the translator successfully, you should be able to run the tests with:

```sh
./scripts/test_translator.py tests/unit
```

This basically tests that the original C file and translated Rust file
produce the same output when compiled and run.
More details about tests can be found in the [tests folder](../tests/).

*Note*: These run integration tests that invoke `c2rust transpile`.
`cargo test` only runs unit tests and doc tests as of now.

## Documentation

Local documentation can be built with the normal `cargo doc` and `cargo doc --open`.

### `rustc`

However, as the `#![feature(rustc_private)]` crates
are not normal dependencies managed through `cargo`,
accessing the documentation for those is trickier.

\* Note: The following `rustup` commands auto-detect the toolchain
from [`rust-toolchain.toml`](../rust-toolchain.toml),
so they must be run inside the workspace.

Also, not all components are always available for every version on every platform.
[rust-lang.github.io/rustup-components-history/](https://rust-lang.github.io/rustup-components-history/)
can be consulted to check this for each toolchain version.

#### `rust-src`

`rust-src` is the `rustup` component containing `rustc`'s source code.
This is needed to view `rustc`'s source when developing.

`rust-src` is included in our [`rust-toolchain.toml`](../rust-toolchain.toml),
so it will automatically be installed.  Otherwise,

```sh
rustup component add rust-src
```

will work.

#### `rustc-docs`

`rustc-docs` (not `rust-docs`) is the `rustup` component
containing the `rustdoc` docs for `rustc`.
This is different from `rust-docs`, the `rustup` component
containing the `rustdoc` docs for `std`, various books on Rust,
and other miscellaneous documentation.

Unfortunately, the `rustc-docs` and `rust-docs` components
are currently mutually incompatible, as they install to the same location:
`share/doc/rust/html`.
See [`rust-lang/rustup#3196`](https://github.com/rust-lang/rustup/issues/3196).

Thus, `rustc-docs` is not included in [`rust-toolchain.toml`](../rust-toolchain.toml)
to avoid any build errors with `rust-docs`, which is installed by default.

To install `rustc-docs`, run

```sh
rustup component remove rust-docs
rustup component add rustc-docs
```

`rustup doc` doesn't work with `rustc-docs`, only with `rust-docs`.
To view it, simply open

```sh
$(rustc --print sysroot)/share/doc/rust/html/rustc/index.html
```

`rustc-docs` for the latest `nightly` are available at
[doc.rust-lang.org/nightly/nightly-rustc/](https://doc.rust-lang.org/nightly/nightly-rustc/),
but as we are not pinned to the latest `nightly`,
the local `rustc-docs` version will be the correct docs.

`rust-docs` is always available for every version, however,
and is far more stable, so that can be accessed online
while the component is removed locally for our `nightly` toolchain:

- [doc.rust-lang.org](https://doc.rust-lang.org/)
- [doc.rust-lang.org/std/](https://doc.rust-lang.org/std/)

#### `rust-analyzer`

To allow `rust-analyzer` to view and analyze `rustc`'s source in VS Code,
add this to your `.vscode/settings.json`:

```json
{
    "rust-analyzer.rustc.source": "discover"
}
```

Also recommended are:

```json
{
    "rust-analyzer.rustc.source": "discover",
    "rust-analyzer.cargo.features": "all",
    "rust-analyzer.checkOnSave.command": "clippy",
    "rust-analyzer.procMacro.attributes.enable": true,
    "cmake.configureOnOpen": true
}
```
