# genann (Neural Network Library)

## Getting Started

If the repo submodule appears to be empty or out of date, you may need to run `git submodule update --init path/to/repo`.

## Transpiling

    # generate compile_commands.json
    $ intercept-build make
    $ c2rust transpile compile_commands.json --emit-build-files

## Testing

Instead of translating with `--emit-build-files` to generate a library crate,
you can build with `--binary exampleN` where `N` is one of 1, 3, or 4
(`example2.c` seems to never halt in both C and Rust but translates and executes
just fine). This will create a binary crate that will run the specified example.
