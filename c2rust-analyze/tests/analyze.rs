use common::Analyze;

pub mod common;

#[test]
fn string_literals() {
    Analyze::resolve().run("tests/analyze/string_literals.rs");
}

#[test]
fn string_casts() {
    Analyze::resolve().run("tests/analyze/string_casts.rs");
}

#[test]
fn lighttpd_minimal() {
    Analyze::resolve().run("../analysis/tests/lighttpd-minimal/src/main.rs");
}
