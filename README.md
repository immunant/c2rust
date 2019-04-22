# c2rust testsuite

# prerequisites

- python 3.7 or later.
- `intercept-build` in path. installing:
    - `pip3 install -r requirements.txt`

# adding new repos

    path/to/repo/$PROJ$ git submodule add --depth 10 -b $BRANCH $PROJ_URL

# TODOs
- [x] check requirements on ubuntu
- [ ] check requirements on macOS
- [ ] warn if `compile_commands.json` is empty
- [ ] add provision.py driven by `**/dependencies.yml`