C2Rust helps you migrate C99-compliant code to Rust. The [translator](c2rust-transpile) (or transpiler) produces unsafe code Rust code that closely mirrors the input C code. The primary goal of the translator is to preserve functionality; test suites should continue to pass after translation. Generating safe and idiomatic Rust code from C ultimately requires manual effort. However, we are building a scriptable [refactoring tool](c2rust-refactor) that reduces the tedium of doing so. You can also [cross-check](cross-checks) the translated code against the original ([tutorial](docs/cross-check-tutorial.md)).

Here's the big picture:

![C2Rust overview](docs/c2rust-overview.png "C2Rust overview")

To learn more, check out our [RustConf'18](https://www.youtube.com/watch?v=WEsR0Vv7jhg) talk on YouTube and try the C2Rust translator online at [www.c2rust.com](https://www.c2rust.com).
