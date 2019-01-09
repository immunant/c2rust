# Setting up a development environment

There are three ways to build the C2Rust project:

- [Using **Vagrant**](../vagrant/).
- [Using **Docker**](../docker/).
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
    - ninja
    - unzip
    - clang 5.0+
    - intercept-build or bear - see why [here](#generating-compile_commandsjson-files)
    - python-dev
    - python 3.6+
    - [python dependencies](../scripts/requirements.txt)
    - rustc [version](../rust-toolchain)
    - rustfmt-preview component for the above rustc version
    - libssl (development library, dependency of the refactoring tool)

# Building with system LLVM libraries

The quickest way to build the C2Rust transpiler is with LLVM and clang system libraries (LLVM 6 and 7 are currently supported). If you have `libLLVM.so` and the `libclang` libraries (`libclangAST.a`, `libclangTooling.a`, etc. or their shared variants) installed, you can build the transpiler with:

    $ cd c2rust-transpile
    $ cargo build

You can customize the location where the build system will look for LLVM using the following environment variables at compile time:

- `LLVM_CONFIG_PATH` = Path to the `llvm-config` tool of the LLVM installation
- `LLVM_LIB_DIR` = Path to the `lib` directory of the LLVM installation (not necessary if you use `LLVM_CONFIG_PATH`)
- `LLVM_SYSTEM_LIBS` = Additional system libraries LLVM needs to link against (e.g. `-lz -lrt -ldl`). Not necessary with `llvm-config`.
- `CLANG_PATH` = Path to a clang that is the same version as your `libclang.so`. If this is necessary the build system will return an error message explaining as much.

C2Rust (indirectly) uses the [clang-sys](https://crates.io/crates/clang-sys) crate which can be configured with its own environment variables.


# Building dependencies from source

To develop on components that interact with LLVM, we recommend building against a local copy of LLVM. This will ensure that you have debug symbols and IDE integration for both LLVM and C2Rust. However, building C2Rust from source with LLVM takes a while. For a shorter build that links against prebuilt LLVM and clang system libraries, you should be able to `cargo build` in the `c2rust-transpile` directory (see the general [README](../)).

The following from source full build script has been tested on recent versions of macOS and Ubuntu:

    $ ./scripts/build_translator.py

TODO: Mention how to open as XCode or other IDE project?

# Testing (Optional)

Tests are found in the [`tests`](../tests/) folder. If you build the translator successfully, you should be able to run the tests with:

    $ ./scripts/test_translator.py tests

This basically tests that the original C file and translated Rust file produce the same output when compiled and run. More details about tests can be found in the [tests folder](../tests/).

