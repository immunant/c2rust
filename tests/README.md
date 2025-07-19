## Adding a test case

To add a new test case, simply create a new `.c` file. For example:

```c
void example(unsigned buffer_size, int buffer[]) {
    /* your code here */
}
```

Then create a new `.rs` file with the following skeleton (_does not need to be a buffer, can check return values as well_):

```rust
use crate::c_file::rust_example;

use std::ffi::c_int;

#[link(name = "test")]
extern "C" {
    fn example(_: c_uint, _: *mut c_int);
}

// The length can be any value
const BUFFER_SIZE: usize = 1024;

#[test]
pub fn test_example() {
    let mut buffer = [0; BUFFER_SIZE];
    let mut rust_buffer = [0; BUFFER_SIZE];
    let expected_buffer = [/* this can be used as another measure of correctness */];

    unsafe {
        example(BUFFER_SIZE as u32, buffer.as_mut_ptr());
        rust_example(BUFFER_SIZE as u32, rust_buffer.as_mut_ptr());
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
```

The C code can do one of two things: modify some sort of buffer or return a value.

To completely skip the translation of a C file, you must add the comment `//! skip_translation` at the top of the file. That will prevent the case from showing up as red in the console output.

You can also mark a Rust file as unexpected to compile by adding `//! xfail` to the top of the file.
For an individual test function, use the normal Rust `#[should_panic]` for `#[test]`s.

Adding `//! extern_crate_X` to the top of a test file will ensure `extern crate X;` gets added to the main binary driver.

Similarly, `//! feature_X` adds `#![feature(X)]` to the top of the main driver file.

## Running the tests

_From the project root_, run `./scripts/test_translator.py tests` to run all of the tests in the
`tests` folder. Here are a couple other handy options:

```shell
# run a subset of the tests
$ ./scripts/test_translator.py --only-directories="loops" tests
# show output of failed tests
$ ./scripts/test_translator.py --log ERROR                tests
# keep all of the files generated during testing
$ ./scripts/test_translator.py --keep=all                 tests
# get help with the command line options
$ ./scripts/test_translator.py --help
```

## What happens under the hood

This `tests` directory contains regression, feature, and unit tests. A test directory goes through the following set of steps:

  1. A `compile_commands.json` file is created for the Clang plugin in `c2rust-ast-exporter` to recognize its C source input

  2. This JSON and the C source file are fed to the `c2rust-ast-exporter` to produce CBOR data of the Clang type-annotated abstract syntax tree.

  3. This CBOR data is fed to the `c2rust-transpile` to produce a Rust source file supposedly preserving the semantics of the initial C source file.

  4. Rust test files (`test_xyz.rs`) are compiled into a single main wrapper and main test binary and are automatically linked against other Rust and C files thanks to `cargo`.

  5. The executable from the previous step is run one or more times parameterized to a specific test function.
