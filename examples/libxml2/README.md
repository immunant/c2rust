# Create a Makefile

in `libxml2/repo`:

`./autogen.sh && ./configure`

You will need to edit the newly generated Makefile. Add these changes:
* `am__libxml2_la_SOURCES_DIST` needs `variadic.c`
* `am_libxml2_la_OBJECTS` needs `variadic.lo`
* `am_runsuite_OBJECTS`, `am_runtest_OBJECTS`, `am_runxmlconf_OBJECTS`, `am_testAutomata_OBJECTS`, `am_testC14N_OBJECTS`, `am_testHTML_OBJECTS`, `am_testModule_OBJECTS`, `am_testReader_OBJECTS`, `am_testRegexp_OBJECTS`, `am_testRelax_OBJECTS`, `am_testSAX_OBJECTS`, `am_testSchemas_OBJECTS`, `am_testThreads_OBJECTS`, `am_testURI_OBJECTS`, `am_testXPath_OBJECTS`, `am_testapi_OBJECTS`, `am_testchar_OBJECTS`, `am_testdict_OBJECTS`, `am_testlimits_OBJECTS`, `am_testrecurse_OBJECTS`, `am_xmlcatalog_OBJECTS` need `variadic.$(OBJEXT)`
* `am_xmllint_OBJECTS` needs `variadic.$(OBJEXT)` and `xmllint_variadic.$(OBJEXT)`

# Create a compile_commands.json

in `libxml2/repo`:

`../../../dependencies/Bear-2.*.*/build/bear/bear make check` so that we can translate all necessary files (including tests).

If your `compile_commands.json` enables optimizations(`-O2`) you will need to remove them so that unsupported compiler_builtins are less likely to be generated and leave you in an uncompilable state.

Run `make clean` here to get rid of gcc staticlibs or else you may see `CRITICAL:root:error: some ELF objects were not compiled with clang:` in the next step

# Generate Rust Code

in `libxml2`:

`./translate.py` to translate all required c files (including tests) into the `libxml2/repo/rust/src` and `libxml2/repo/rust/examples` directories.

# Fix Known Issues

in `libxml2`:

`./patch_translated_code.py` to step through applying patches to some known issues in the generated code.

# Run libxml2 c tests

Since each of these tests have their own main file, we decided to move them to the rust examples directory instead of trying to wrap them in the test framework.

You can run a test like so: `cargo run --example EXAMPLE` where `EXAMPLE` is one of the files in `libxml2/repo/rust/examples`, not including the file extension.

# Known Issues

Coming soon
