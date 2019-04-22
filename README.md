# C2Rust testsuite

## prerequisites

- `c2rust` binary in path
- python 3.7 or later.
- `intercept-build` in path. installing:
    - `pip3 install -r requirements.txt`

## testing

    $ ./test.py            # test everything
    $ ./test.py --verbose  # show test output
    $ ./test.py --only lua # run specific test
    

## adding new repos as git submodules

    $ cd path/to/repos/$PROJ
    $ git submodule add --depth 10  $PROJ_URL repo
    $ git config -f .gitmodules submodule.$SUBMOD_NAME.branch $BRANCH_NAME
    $ git submodule update --remote
    
## scripting test steps

Each test stage can be controlled with a script (in `repos/$PROJ`) named as follows:

- configure stage -> `configure.sh`
- make stage ->  `make.sh` | `cmake.sh`
- transpile stage ->  `transpile.sh` | `cmake.sh`
- cargo build stage -> `cargo.sh`
- test stage -> `check.sh` | `test.sh`

Each script is expected to `tee` its output to a file named `$SCRIPT.log`. For example, `make.sh` produces `make.sh.log`.

# TODOs
- [x] check requirements on ubuntu
- [ ] check requirements on macOS
- [ ] warn if `compile_commands.json` is empty
- [ ] add provision.py driven by `**/dependencies.yml`
