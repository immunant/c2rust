# lil (Little Interpreted Language)

## Getting Started

If the repo submodule appears to be empty or out of date, you may need to run `git submodule update --init path/to/repo`.

## Transpiling

    $ bear make
    $ c2rust transpile compile_commands.json --emit-build-files -m main
    $ cd c2rust-build
    $ cargo build
