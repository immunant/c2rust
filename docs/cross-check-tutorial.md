# Cross-checking Tutorial
## Introduction
The C2Rust transpiler aims to convert C code to semantically equivalent unsafe Rust code,
and later incremental refactoring passes gradually transform this code to Rust code.
However, the initial Rust translation might not be a perfect semantic match to the original C code,
and the refactoring passes may also change the code in ways that break semantics.
Cross-checking is an automated way to verify that the translated program behaves the same as the original C code.

The way cross-checking achieves this goal is by comparing the execution traces of all versions (henceforth called "variants") of the program (original C, unsafe and refactored Rust) and checking for any differences.
Our cross-checking implementation modifies the source code of the program at compile-time (separately during C and Rust compiler invocation) so that the variants output the traces at run-time, and then checks the traces against each other either online during execution (using the ReMon MVEE), or offline by comparing log files.
The C2Rust cross-checkers currently instrument function entry and exit points, function return values, 
and function call arguments (currently experimental and disabled by default, but can be enabled per argument, function or file).

## Building code with cross-checks
C2Rust contains one cross-checking implementation per language, in the form of a compiler plugin in both cases.
We provide a clang plugin for C code, and a rustc plugin for Rust code.

### Building C code
To build C variants with cross-checks enabled, first build the cross-checking plugin using `$C2RUST/scripts/build_cross_checks.py`, 
then run clang (or pass it to the build system) with the following options:
  * `-Xclang -load -Xclang $C2RUST/dependencies/clang-xcheck-plugin.$(uname -n)/plugin/CrossChecks.so` to load the plugin
  * `-Xclang -add-plugin -Xclang crosschecks` to activate the plugin
  * `-Xclang -plugin-arg-crosschecks -Xclang <...>` for every additional option to pass to the plugin
  * `-ffunction-sections` may be required to correctly deduplicate some linkonce functions inserted by the plugin
  
Note that every option passed to clang requires a `-Xclang` prefix before the actual option, 
so that the compiler driver passes it to the clang backend correctly.
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
When cross-checking in offline mode, all variants are executed independentely on the same inputs, and their cross-checks are written to either standard output or log files.
After running all the variants, divergence can be detected by manually comparing the logs for mismatches.
There are several backend libraries that support different types of logging outputs:
  * `libfakechecks` outputs a list of the cross-checks linearly to either standard output or a file 
  (specified using the `FAKECHECKS_OUTPUT_FILE` environment variable)
  * `fakechecks-zstd` library from `cross-checks/rust-checks/backends` (can also be used with the clang plugin) 
  outputs a binary encoding of the cross-checks that is compressed using zstd, and is much more space-efficient than 
  the text output of `libfakechecks`. The compressed output files can be converted to text using the `xcheck-printer` tool.
  
Before running the C and Rust variants, you may need to load in one of these libraries using `LD_PRELOAD` if you 
haven't linked against it and passed in its path using `-rpath` (this is fairly easy to do for a C build, but 
more complicated when using Cargo for Rust code), like this:
```Bash
$ env LD_PRELOAD=$C2RUST/cross-checks/libfakechecks/libfakechecks.so ./a.out
```

Running each variant with cross-checks enabled will print a list of cross-check results to the specified output. A simple `diff` or `cmp` command will show differences in cross-checks, if any.

### Online (MVEE) mode
The other execution mode for cross-checks is the online mode, where a monitor program (the MVEE) runs all variants in parallel with exactly the same inputs (by intercepting input system calls like `read` and replicating their return values) and cross-checks all the output system calls and instrumentation points inserted by our plugins. This approach has several advantages over offline mode:
  * Input operations are fully replicated, including those from stateful resources like sockets; only the master variant performs each actual operation, and each other variant only gets a copy of the data.
  * Outputs are cross-checked but not duplicated, so each output operation is only executed by the master variant; the others are only cross-checked for matching outputs. For example, only the master variant opens and writes to output files.
  * The lock-step MVEE automatically eliminates most sources of non-determinism, like threading and non-deterministic syscalls, e.g., reading from `/dev/urandom` (see the Troubleshooting section below for more details)

However, the main disadvantage of this approach is that some applications may not run correctly under the MVEE, due to either incomplete support from the MVEE or fundamental MVEE limitations. In such cases, we recommend using offline mode instead.

To run your application inside our MVEE, first build it following the instructions in its [README](https://github.com/stijn-volckaert/ReMon/blob/master/README.md). After building it successfully, write an MVEE configuration file for your application (there is a [sample file](https://github.com/stijn-volckaert/ReMon/blob/master/MVEE/bin/Release/MVEE.ini) in the MVEE directory, and a few others in our [examples](../examples) directory), then run the MVEE:
```Bash
$ ./MVEE/bin/Release/MVEE -f <path/to/MVEE_config.ini> -N<number of variants> -- <variant arguments>
```

The `MVEE.ini` configuration file is fairly self-explanatory, but there are a few notable settings that are important:
  * `xchecks_initially_enabled` disables system call replication and cross-checks up to the first function cross-check (usually for the `main` function), and should be `false` by default for cross-language checks. This is because the Rust runtime performs a few additional system calls that C code does not, and the MVEE would terminate with divergence if cross-checks were enabled.
  * `relaxed_mman_checks` and `unsynced_brk` disable MVEE cross-checks on the `mmap` family of calls and `brk`, respectively, and should both be set to `true` if the Rust code performs significantly different memory allocations.
  * both the global and per-variant `env` variable contain the environment variables to pass to the variants, and should at least contain a `LD_LIBRARY_PATH` entry for the `libclevrbuf.so` library, and a `LD_PRELOAD` entry for the zeroing allocator `libzero_malloc.so`, like this:
```JSON
{
  "variant": {
    "global": {
      "exec": {
        "env": [
          "LD_LIBRARY_PATH=../../../cross-checks/ReMon/libclevrbuf",
          "LD_PRELOAD=../../../cross-checks/zero-malloc/target/release/libzero_malloc.so"
        ]
      }
    }
  }
}
```

## Troubleshooting
In case you run into any issues while building or running the variants, please refer to this section for possible fixes.

### Build failures
Builds may occasionally fail because of partially or completely unsupported features in our plugins:
  * Bitfields in C structures: these do not currently have a Rust equivalent, and the transpiler converts them to regular integer types. The clang plugin will exit with an error when trying to cross-check a bitfield.
  * Variadic functions: Rust does not support these yet, and our clang plugin cannot handle them either.
  * Fixed-sized arrays of large or unusual sizes: as of the writing of this document, Rust does not have const generics yet, and we need them to support arbitrary-sized arrays on the Rust side. Until then, the rustc plugin runtime only supports cross-checking on fixed-sized arrays from a limited set of sizes (all integers up to 32, all powers of 2 up to 1024).
  * Function pointers with more than 12 arguments: these require variadic generics in Rust.
  
In all these cases, we recommend that you either disable cross-checks for values of these types, or manually provide a custom cross-check function (see the [cross-check configuration](cross-check-config.md) for more details).

### Inline functions in C headers
Cross-checker output for C variants may sometimes contain additional cross-checks for inline functions from system headers which are missing from the corresponding Rust translation. In such cases, we recommend manually disabling cross-checks for the C inline functions using an external cross-check configuration file.

### Non-determinism
Ideally, divergence (cross-checks differences between variants) is only caused by actual semantic differences between the variants. However, there is another cause of unintended divergence: nondeterminism in program execution. In most cases, non-determinism will simply cause each variant to produce different cross-checks, but it may also occasionally cause crashes.  There are many causes of program non-determinism that interfere with cross-checking:
  * Calls to time, RNG, PID (`getpid()` and friends) and other non-deterministic OS functions and operations, e.g., reads from `/dev/urandom`
  * File descriptors. We have hard-coded a fixed cross-check value for all `FILE*` objects for this exact reason.
  * Threading, i.e., non-deterministic thread scheduling
  * Pointer-to-integer casts under ASLR.
  
Generally, running the variants in online mode inside the MVEE fixes these issues by replicating all system calls between the variants, which ensures that they all receive the same values from the OS.
In case the MVEE does not support a specific application and you need to run it in offline mode (or for any other reason), the recommended fix is to remove all such non-determinism from your code manually, e.g., replace all reads from `/dev/urandom` with constant values.

To verify that non-determinism truly is the cause of divergence, we recommend running each separate variant multiple times and cross-checking it against itself. If non-determinism really is the problem, each run will produce different cross-checks.

A note on ASLR and pointers: while our cross-checkers currently check pointers by dereference, and not by address, which makes the checks insensitive to ASLR, manually casting pointers to integers poses problems.
Integers are cross-checked by value regardless of their source, and their values will differ across runs when they originate from pointers with ASLR enabled.

### Uninitialized memory
Uninitialized memory is one common source of non-determinism, since an uninitialized value may have different actual values across different runs of a program.
Since our plugins cross-check pointers by dereferencing them, invalid pointers can also crash our cross-checking runtime.

To eliminate this problem, we force zero-initialization for all C and Rust values.
The plugins enforce this for stack (function-local values), and all global values are already zero-initialized (as required by the C standard), which only leaves heap-allocated values, i.e., those allocated using `malloc`.
We provide a zeroing `malloc` wrapper under `cross-checks/zero-malloc` which can be preloaded into an application using `LD_PRELOAD`. This wrapper library intercepts all memory allocation calls, and zeroes the allocated buffer after each call. To use this in Rust executables in place of the default `jemalloc`, add the following lines to your code to use the system allocator, which our library intercepts:
```Rust
#[global_allocator]
static A: ::std::alloc::System = ::std::alloc::System;
```

We recommend that you use our zeroing memory allocator for all cross-checks.

### Pointer aliasing
Some C code will use pointers of some type `T` to refer to values of another type `U`, tricking our runtime into cross-checking the values incorrectly.
This may not only cause divergence, but also potential crashes when our runtime attempts to cross-check actual integers as invalid pointers (see example below). If the value of the integer incidentally represents a valid memory address, the runtime will try to cross-check that memory as a `T`; otherwise, the runtime will most likely crash.
```C
struct T {
  int n;
};
struct U {
  char *s;
};
void foo(struct U *x) {
  // Cross-check problem here: will try to cross-check `x` as a `struct U*`,
  // when it's a `struct T*`, so the check will most likely crash
  // when attempting to dereference x->s
}
int main() {
  T x = { 0x1234 };
  foo((struct U*)&x);
  return 0;
}
```

Our cross-checking runtimes can recover from attempts to dereference invalid pointers, but rely on the `pointer-tracer` tool  that uses `ptrace` to check and restart all invalid pointer dereferences.
To use this recovery feature, you must `pointer-tracer` to start the variants:
```Bash
$ $C2RUST/cross-checks/pointer-tracer/target/release/pointer-tracer ./a.out
```

Alternatively, some issues caused by pointer aliasing can be fixed by disabling cross-checks altogether for certain types and values, or by providing custom cross-check functions for certain types. For example, one common pattern is the *tagged union*, where multiple structures have an overlapping prefix with a tag, followed by a type-specific part:
```C
enum Tag {
  TAG_A,
  TAG_B
};
struct TypeA {
  // Common part
  Tag tag;
  char *foo;
  // Specific part
  int valA;
};
struct TypeB {
  // Common part
  Tag tag;
  char *foo;
  // Specific part
  void *valB;
};
```
For this example, you can either disable cross-checks manually for all the type-specific types, e.g., `valA` and `valB` above, or provide a custom cross-check function that cross-checks each value based on its tag.

### End-of-buffer pointers
Another common C pattern related to memory allocations is the *pointer to end of buffer* pattern:
```C
struct Buf {
  char *start;
  char *end;
};
void alloc(struct Buf *buf, unsigned size) {
    buf->start = malloc(size);
    buf->end = buf->start + size;
}
```
In this example, cross-checks will diverge on the `end` pointer, since it points to the first byte after the allocation returned by `malloc`. Since we only require the allocation itself to be zero-initialized, the value of that byte is undefined, and could change at any time during the execution of the variant.

For any pointers outside allocated memory, we recommend disabling cross-checks altogether.

### Benign dangling pointers
The cross-checking runtimes may attempt to cross-check pointers to values that have been deallocated, e.g., by calling `free`, but still linger in memory without being used by the program.
This means that the runtimes may dereference these pointers, even if the program never does, which may lead to divergence since the allocator is free to reuse that memory.
```C
struct Data {
  int allocated;
  char *buf;
};
struct Data *free_data(struct Data *data) {
    data->allocated = 0;
    free(data->buf);
    // Potential non-determinism: data->buf has been freed,
    // but our runtime will try to dereference it
    return data;
}
```
As of the writing of this document, we have no automatic way to detect when the runtimes attempt to dereference deallocated memory, so we recommend manually disabling cross-checks when this occurs.
