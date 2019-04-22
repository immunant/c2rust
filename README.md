# c2rust testsuite

# prerequisites

- python 3.7 or later.
- `intercept-build` in path. installing:
    - `pip3 install -r requirements.txt`

# adding new repos

    $ cd path/to/repos/$PROJ
    $ git submodule add --depth 10  $PROJ_URL repo
    $ git config -f .gitmodules submodule.$SUBMOD_NAME.branch $BRANCH_NAME
    $ git submodule update --remote

# TODOs
- [x] check requirements on ubuntu
- [ ] check requirements on macOS
- [ ] warn if `compile_commands.json` is empty
- [ ] add provision.py driven by `**/dependencies.yml`
