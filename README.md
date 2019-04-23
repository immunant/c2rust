# C2Rust testsuite

## prerequisites

- `c2rust` binary in path
- python 3.6 or later.
- `intercept-build` in path. installing:
    - `pip3 install -r requirements.txt`
- any other packages and binaries required by the tests themselves
    - captured in `**/requirements.yml`

## testing

    $ ./test.py                 # test everything
    $ ./test.py --verbose       # show test output
    $ ./test.py --project lua   # run specific project
    

## adding new tests as git submodules

    $ cd path/to/tests/$PROJ
    $ git submodule add --depth 10  $PROJ_URL repo
    
To track a specific branch ($SUBMOD_NAME can be found in `.gitmodules`):    
    
    $ git config -f .gitmodules submodule.$SUBMOD_NAME.branch $BRANCH_NAME
    $ git submodule update --remote repo
    
## scripting test steps

Each test stage can be controlled with a script (in `tests/$PROJ`) named as follows:

- autogen stage -> `autogen.sh`
- configure stage -> `configure.sh`
- make stage ->  `make.sh` | `cmake.sh`
- transpile stage ->  `transpile.sh` | `cmake.sh`
- cargo build stage -> `cargo.sh`
- test stage -> `check.sh` | `test.sh`

Each script is expected to `tee` its output to a file named `$SCRIPT.log`. For example, `make.sh` produces `make.sh.log`.

# TODOs
- [x] check requirements on ubuntu
- [x] rename flag `--only` to `--project`
- [ ] check requirements on macOS
- [x] `requirements.yml` -> `conf.yml` 
  - [x] make requirements a key.
  - allow cargo build and transpile steps w/o scripts
- [ ] warn if `compile_commands.json` is empty
- [ ] add provision.py driven by `**/conf.yml`
- [ ] add option to run `c2rust transpile` under `rust-gdb`
