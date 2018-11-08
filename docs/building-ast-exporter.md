**NOTE:** On macOS and Linux, you can use `build_translator.py` to perform these steps.

Building the c2rust-ast-exporter currently requires an LLVM project checkout.
I haven't sliced the portion of the CMake build system that LLVM uses
to build its tools. These instruction use the directory `~/clang-llvm`
because the tutorial does.

This build process is based on the following tutorial:
<https://clang.llvm.org/docs/LibASTMatchersTutorial.html>

CMake supports multiple build system targets. The tutorial uses `ninja`
and locally I've been using XCode to build.

The exporter uses *tinycbor* for CBOR serialization: <https://github.com/01org/tinycbor> I installed this into
`/usr/local` to make it available locally.

Initial source checkout
======================

```
mkdir ~/clang-llvm && cd ~/clang-llvm
git clone http://llvm.org/git/llvm.git
cd llvm/tools
git clone http://llvm.org/git/clang.git
cd clang/tools
git clone http://llvm.org/git/clang-tools-extra.git extra
```

Here's where I added the c2rust-ast-exporter source directory to the llvm build tree.

```
$ cd ~/clang-llvm/llvm/tools/clang/tools/extra
$ echo 'add_subdirectory(c2rust-ast-exporter)' >> CMakeLists.txt
$ ln -s $PATH_TO_ast_exporter
```

Building with Ninja
===================

Ninja is a command-line too that seems similar to make.

```
cd ~/clang-llvm
mkdir build && cd build
cmake -G Ninja ../llvm -DLLVM_BUILD_TESTS=ON  # Enable tests; default is off.
ninja
ninja check       # Test LLVM only. (optional)
ninja clang-test  # Test Clang only. (optional)
ninja c2rust-ast-exporter
```

Building with XCode
===================


```
$ cd ~/clang-llvm
$ mkdir build-xcode
$ cd build-xcode
$ cmake -G Xcode ../llvm/
$ open LLVM.xcodeproj
```

Debugging the AST Exporter
===========================

The `c2rust-ast-exporter` uses [LLVMs debug macros](http://llvm.org/docs/ProgrammersManual.html#the-debug-macro-and-debug-option). To enable debug output add `-debug-only=c2rust-ast-exporter` to the command line invocation.
