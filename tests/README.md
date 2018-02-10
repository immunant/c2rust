## Adding a test case

To add a new test case, simply add a new `.c` file to this directory. The entry point should _not_
be main. It should be a function `entry` which expects to receive a zero-initialized buffer array.

```c
    void entry(unsigned buffer_size, int buffer[]) {
        /* your code here */
    }
```

Your C code should modify this buffer. We simply test that the translated Rust `entry` function
modifies the buffer in the _same_ way as the initial C `entry` function. Bugs in translation should
be reduced to cases where the translated Rust `entry` function does _not_ modify the buffer in the
same way as the initial C `entry` function.

If you want a test case to be marked as expected to fail, include a comment on the first line of
the C file with the word `fail` somewhere in it. That will prevent the case from showing up as red
in the console output.

## Running the tests

_From the project root_, run `./scripts/test_translator.py tests` to run all of the tests in the
`tests` folder. Here are a couple other handy options:

```
    $ ./scripts/test_translator.py --only "q?sort"    tests    # run a subset of the tests
    $ ./scripts/test_translator.py --log ERROR        tests    # show output of failed tests
    $ ./scripts/test_translator.py --keep rust_src    tests    # keep some of the generated files
```

Run `./scripts/test_translator.py --help` for more information.

## What happens under the hood

This `test` directory contains regression / feature / unit tests. Tests go through the following set
of steps

  1. a `compile_commands.json` file is created for the Clang plugin in `ast-extractor` to recognize
     its C source input

  2. this JSON and the C source file are fed to the `ast-extractor` to produce a CBOR file of the
     Clang type-annotated abstract syntax tree

  3. this CBOR file is fed to the `ast-importer` to produce a Rust source file supposedly preserving
     the semantics of the initial C source file

  4. this Rust source file is compiled into a static library by (an unstable version of) `rustc`

  5. [a C driver program](../scripts/driver.c) is compiled (using Clang) and linked with the static
     library

  6. the same C driver program is compiled (using Clang) with the C source file (from step 2)

  7. the executables from steps 5 and 6 are both run and their outputs are compared for differences
