# tmux

## Checking out the tmux sources

Only linux is supported at the moment, but OSX might work with some tweaks.

In `path/to/examples/tmux`, initialize the git submodule:

`git submodule update --init repo`

## Create a Makefile

in `tmux/repo`:

`./autogen.sh && ./configure`

## Create a compile_commands.json

in `tmux/repo`:

`intercept-build make check`

If your `compile_commands.json` enables optimizations(`-O2`, `-O3`, etc) you will need to remove them so that unsupported compiler_builtins are less likely to be generated and leave you in an uncompilable state.

Run `rm *.o compat/*.o` here to get rid of gcc generated staticlibs or else you may see `CRITICAL:root:error: some ELF objects were not compiled with clang:` in the next step

## Generate Rust Code

in `tmux`:

`./translate.py` to translate all required c files into the `tmux/repo/rust/src` and `tmux/repo/rust/src/compat` directories.

## Run Tmux

Run `cargo run` to build and execute tmux.
