# Snudown
To build snudown with the C2Rust translator and/or cross-checks, initialize the git submodule by running `git submodule update --init path/to/repo`. Next, `cd` into the `repo` directory and run `python setup.py build` with one of the following arguments:
* `--translate` to translate the C code to Rust without any checks
* `--clang-crosschecks` to build the C version of snudown with full cross-checking
* `--rust-crosschecks` to translate to cross-checked Rust code
* `--use-fakechecks` may be appended to use the `fakechecks` library to print out the cross-checks, instead of `libclevrbuf` from the MVEE
* running with no flags will build the C version of the code
* Note that `-f` may need to be appended to the end of the command to force a rebuild, if building multiple times consecutively

After building any of the 3 versions, run `python setup.py test` to test it.
