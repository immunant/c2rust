# Generate compile_commands.json

in libxml2/repo:
`./autogen.sh && ./configure && ../../../dependencies/Bear-x.y.z/build/bear/bear make`

You may need to remove optimizations `-O2` from your compile_commands.json so that unsupported compiler_builtins are less likely to be generated and mistranslated.
You may also need to `make clean` here if you see `CRITICAL:root:error: some ELF objects were not compiled with clang:` in the next step

# Generate Rust Code

in libxml2:
`./translate.py` to translate all required c files into the `libxml2/repo/rust` directory (it may exclude tests at the moment).

# Fix Known Issues

in libxml2:
`./patch_translated_code.py` to step through applying patches to some known issues in the generated code.
