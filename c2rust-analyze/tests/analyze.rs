use common::Analyze;

pub mod common;

#[test]
fn string_literals() {
    Analyze::new().run("tests/analyze/string_literals.rs");
}

#[test]
fn string_casts() {
    Analyze::new().run("tests/analyze/string_casts.rs");
}

#[test]
fn lighttpd_minimal() {
    Analyze::new().run("../analysis/tests/lighttpd-minimal/src/main.rs");
}
