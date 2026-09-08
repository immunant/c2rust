# SQLite

The source submodule is pinned to SQLite 3.53.4 from the official Git mirror.
Initialize it with:

```sh
git submodule update --init --checkout tests/integration/tests/sqlite/repo
```

Run from the C2Rust checkout with the integration harness prerequisites installed:

```sh
export PATH="$PWD/target/release:$PWD/c2rust-postprocess:$PATH"
export C2RUST_DIR="$PWD"
./tests/integration/test.py sqlite
```

The build uses `--disable-amalgamation` and the `libsqlite3.a` target to compile
the library's individual translation units. SQLite's `sqlite3` and `speedtest1`
make targets compile the amalgamation even with that configure option, so they
are deliberately not used. Source generation runs before Bear to exclude build
tools from the compilation database. R-tree support is enabled for its workload.

