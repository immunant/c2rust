```sh
rustc --crate-type rlib instrument_support.rs
cargo run -- fib.rs -L ~/.rustup/toolchains/nightly-2022-02-14-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/
./fib
```

The final `./fib` command should print several lines like
```
[CALL] fib(5,)
```
which come from the injected calls to `instrument_support::handle_call`.
