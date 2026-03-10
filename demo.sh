cargo run --bin c2rust-transpile -- --overwrite-existing math_demo.c
cargo run --bin c2rust-refactor -- remove_redundant_let_types --rewrite-mode inplace -- math_demo.rs --edition 2021
rustfmt math_demo.rs
cargo run --bin c2rust-refactor -- remove_redundant_casts --rewrite-mode inplace -- math_demo.rs --edition 2021
cargo run --bin c2rust-refactor -- convert_math_funcs --rewrite-mode inplace -- math_demo.rs --edition 2021
rustc math_demo.rs --crate-type lib --edition 2021 --crate-name whatever
