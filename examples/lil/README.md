# lil (Little Interpreted Language)

## Getting Started

If the repo submodule appears to be empty or out of date, you may need to run `git submodule update --init path/to/repo`.

## Transpiling

    $ intercept-build make
    $ c2rust transpile compile_commands.json --emit-build-files -m main --output-dir rust
    $ cd rust
    $ cargo build
