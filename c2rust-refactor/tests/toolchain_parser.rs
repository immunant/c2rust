#![feature(rustc_private)]

mod common;

use common::{command, compile_and_run, refactor, Fixture, COMPILER};
use std::fs;
use std::process::Command as Process;

#[test]
fn malformed_statement_replacement_preserves_parser_recovery() {
    let _compiler = COMPILER.lock().unwrap();
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let main = fixture.write(
            "main.rs",
            "fn main() { let old = 1; println!(\"{}\", 42); }\n",
        );
        // The old parse_stmt emits a diagnostic and returns no statement.
        // Refactoring accepts that result, leaving subsequent statements intact.
        refactor(
            &main,
            edition,
            vec![command(
                "rewrite_stmts",
                &["let old = 1;", "parse!(let =);"],
            )],
        );
        let rewritten = fs::read_to_string(&main).unwrap();
        assert!(!rewritten.contains("let old"), "{rewritten}");
        assert_eq!(compile_and_run(&main, edition), "42\n");
    }
}

#[test]
fn malformed_expression_replacement_consumes_its_diagnostic_before_unwinding() {
    let fixture = Fixture::new();
    let main = fixture.write("main.rs", "fn main() { let _value = 1; }\n");
    let output = Process::new(env!("CARGO_BIN_EXE_c2rust-refactor"))
        .current_dir(&fixture.0)
        .args(["rewrite_expr", "1", "parse!(let =)", "--"])
        .arg(&main)
        .args(["--edition", "2024"])
        .output()
        .unwrap();
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Failed to parse Expr"), "{stderr}");
    assert!(
        !stderr.contains("diagnostic was created but not emitted"),
        "{stderr}"
    );
    assert!(!stderr.contains("panic in a destructor"), "{stderr}");
}
