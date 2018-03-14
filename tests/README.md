## Adding a test case

To add a new test case, simply create a new `.c` function with any return type, name, and parameters.

```c
void example(unsigned buffer_size, int buffer[]) {
    /* your code here */
}
```

Then create a new `.rs` file with the following skeleton(_does not need to be a buffer, can check return values as well_):

```rust
extern crate libc;

use c_file::rust_example;

use self::libc::c_int;

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn example(_: c_uint, _: *mut c_int);
}

// The length can be any value
const BUFFER_SIZE: usize = 1024;

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

So, your C code can do two things: either modify some sort of buffer or have a return a value.

To completely skip the translation of a C file, you must add the comment `//! skip_translation` at the top of the file. That will prevent the case from showing up as red in the console output.

You can also mark a rust file as unexpected to compile, by adding `//! xfail` to the top of the file, or just expect an individual test function to fail to run by adding `// xfail` prior to the function definition.

## Running the tests

_From the project root_, run `./scripts/test_translator.py tests` to run all of the tests in the
`tests` folder. Here are a couple other handy options:

```bash
$ ./scripts/test_translator.py --only-directories="loops" tests    # run a subset of the tests
$ ./scripts/test_translator.py --log ERROR                tests    # show output of failed tests
$ ./scripts/test_translator.py --keep=all                 tests    # keep some of the generated files
$ ./scripts/test_translator.py --help                              # displays the help messages
```


## What happens under the hood

This `test` directory contains regression / feature / unit tests. A test directory goes through the following set of steps:

  1. A `compile_commands.json` file is created for the Clang plugin in `ast-extractor` to recognize
     its C source input

  2. This JSON and the C source file are fed to the `ast-extractor` to produce a CBOR file of the
     Clang type-annotated abstract syntax tree

  3. This CBOR file is fed to the `ast-importer` to produce a Rust source file supposedly preserving
     the semantics of the initial C source file

  4. Rust test files (test_xyz.rs) are compiled into a single main wrapper and main test binary and are automatically linked against other rust and c files thanks to rustc

  5. The executable from the previous step is run one or more times parameterized to a specific test function
