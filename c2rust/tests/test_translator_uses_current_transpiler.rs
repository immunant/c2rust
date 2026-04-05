use std::process::Command;

#[test]
fn test_translator_uses_current_transpiler() {
    let transpile_path = env!("CARGO_BIN_EXE_c2rust-transpile");

    let status = Command::new("python")
        .arg("../scripts/test_translator.py")
        .arg("../tests/unit")
        .arg("--transpiler")
        .arg(transpile_path)
        .status()
        .expect("failed to run python script");

    assert!(
        status.success(),
        "test_translator.py failed with status: {}",
        status
    );
}
