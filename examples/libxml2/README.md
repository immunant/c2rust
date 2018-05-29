# Generate compile_commands.json
in libxml2/repo:
./autogen.sh && ./configure && ../../../dependencies/Bear-x.y.z/build/bear/bear make

You may need to remove optimizations `-O2` from your compile_commands.json so that unsupported compiler_builtins are less likely to be generated and mis translated.

