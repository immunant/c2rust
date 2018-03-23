# Snudown
To build snudown with the C2Rust translator and/or cross-checks, go into the `repo` directory and run `python setup.py build` with one of the following arguments:
* `--clang-crosschecks` to build the C version of snudown with full cross-checking
* `--translator` to translate the C code to Rust without any checks
* `--rust-crosschecks` to translate to cross-checked Rust code

After building any of the 3 versions, run `python setup.py test` to test it.
