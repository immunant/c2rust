# SQLite

The source submodule is pinned to SQLite 3.53.4 from the official Git mirror.
Initialize it with:

```sh
git submodule update --init --checkout tests/integration/tests/sqlite/repo
```

Run from the C2Rust checkout with the integration harness prerequisites and `jq`
installed:

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

After transpilation and again after refactoring, `test.sh` compiles SQLite's
upstream `test/speedtest1.c` against each of the native and translated static
libraries. It runs the `main`, `cte`, `orm`, `fp`, and `rtree` workloads with
`--size 1 --verify` on temporary on-disk databases.
Each workload must exit successfully and produce the same result byte count and
verification hash as native SQLite. Timing output is ignored. The C test driver
stays native so the same caller exercises both libraries' public C ABI.
The Rust archive's native link dependencies come from rustc's
`--print native-static-libs` output.

`smoke.c` also checks fixed SQL results, transaction/savepoint rollback, triggers,
foreign keys, blobs, JSON/JSONB operations, numeric parsing (including large
integers and floating-point values), window functions, data and WAL journal-mode
persistence after reopening, and an integrity check. The upstream `json` and
`parsenumber` workloads do not hash result rows, and `parsenumber` ignores SQL
errors, so JSON and numeric parsing use these explicit assertions instead.
