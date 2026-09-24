use std::process::Command;

#[test]
fn test_translator() {
    let transpile_path = env!("CARGO_BIN_EXE_c2rust-transpile");

    let mut cmd = Command::new("../scripts/test_translator.py");
    cmd.args(["../tests/unit", "--transpiler", transpile_path]);
    
    // `test_translator.py` needs to be able to re-select the toolchain for different editions.
    cmd.env_remove("RUSTUP_TOOLCHAIN");

    let status = cmd
        .status()
        .unwrap_or_else(|e| panic!("{cmd:?} failed: {e}"));

    assert!(status.success(), "{cmd:?} failed with {status}");
}
