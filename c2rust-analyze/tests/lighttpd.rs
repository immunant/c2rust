use common::Analyze;

pub mod common;

#[test]
fn test_lighttpd_minimal() {
    Analyze::new().run("../analysis/tests/lighttpd-minimal/src/main.rs");
}
