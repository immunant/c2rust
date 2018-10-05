# Checking out the libxml2 sources

In `path/to/examples/libxml2`, initialize the git submodule: 

`git submodule update --init repo`

# Create a Makefile

in `libxml2/repo`:

`./autogen.sh`

and optionally `./configure` (`autogen.sh` currently runs this automatically,
so you're not required to).

# Create a compile_commands.json

in `libxml2/repo`:

`bear make check` 

if `bear` is in `$PATH`. Otherwise run:

`../../../dependencies/Bear.$HOSTNAME/bin/bear make check` so that we can translate all necessary files (including tests).

If your `compile_commands.json` enables optimizations(`-O2`) you will need to remove them so that unsupported compiler_builtins are less likely to be generated and leave you in an uncompilable state.

Run `rm .libs/*.o` here to get rid of gcc generated staticlibs or else you may see `CRITICAL:root:error: some ELF objects were not compiled with clang:` in the next step

# Generate Rust Code

in `libxml2`:

`./translate.py` to translate all required c files (including tests) into the `libxml2/repo/rust/src` and `libxml2/repo/rust/examples` directories.

# Fix Known Translation Issues

in `libxml2`:

`./patch_translated_code.py` to apply patches to some known issues in the generated code.

# Run Libxml2 C Tests

Since each of these tests have their own main file, we decided to move them to the rust examples directory instead of trying to wrap them in the test framework.

You can run a test like so: `cargo run --example EXAMPLE` where `EXAMPLE` is one of the files in `libxml2/repo/rust/examples`, not including the file extension.

# Outstanding Test Issues

## Runnable

* `testReader` seems to be mostly working identically but with some slight differences. Try `testReader --valid test/japancrlf.xml`. It produces an extra "Ns verBoom: Validation failed: no DTD found !, (null), (null)"

## Working

* `runtest` seems to be consistently successful now
* `testRelax` seems to work equivalently with files as in C
* `testXPath` seems to work equivalently with files as in C
* `xmllint` seems to work equivalently with files as in C
* `testSAX` prints out nothing on success, just like C version
* `testModule` prints "Success!"
* `testHTML` works with input files from `test/HTML` and produces same output as C version
* `testRegexp` works with files from `test/regexp` and produces same output as C version
* `testrecurse` prints "Total 9 tests, no errors"
* `testlimits` prints "Total 514 tests, no errors"
    * Note: text output seems noticeably slower than the C version
* `testThreads` prints nothing (but no longer prints parsing errors)
* `testapi` runs successfully and prints "Total: 1172 functions, 280928 tests, 0 errors"
* `testC14N` prints parsed output when given a file to read from `test/c14n`
* `testSchemas` no longer crashes when provided a file from `test/schemas/*.xsd`
* `testchar` prints tests completed
* `testdict` prints "dictionary tests succeeded 20000 strings"
* `testAutomata` takes a file from `test/automata` and produces equivalent output to C run
* `testURI` waits on input from stdin, needs example input from `test/URI`. See `Makefile.am` and `result/URI/uri.data` for examples

## Working cross-checks
* `testchar` all cross-checks match
* `testdict` all cross-checks match
* `testapi` all cross-checks match (345 million)
* `runtest` all cross-checks match
* `testlimits` all cross-checks match, but requires `-fno-builtin` as a compiler argument
* `testSAX` works
* `testHTML` works
* `testRegexp` works
* `testModule` requires `testdso.so`, doesn't work yet
* `testAutomata` works
* `testSchemas` works on all files from `test/schemas`
* `testRelax` works on all files from `test/relaxng`
* `testURI` works
* `testC14N` works
* `testXPath` works on files under `test/XPath/expr` and `test/xmlid`
* `testThreads` deadlocks, still investigating
* `xmlllint` does not compile

