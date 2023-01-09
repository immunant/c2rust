use std::process::Command;

#[test]
fn test_lighttpd_minimal() {
    let dir = env!("CARGO_MANIFEST_DIR");
    let lib_dir = env!("C2RUST_TARGET_LIB_DIR");
    let mut cmd = Command::new("cargo");
    let path = "analysis/tests/lighttpd-minimal/src/main.rs";
    cmd.arg("run")
        .arg("--manifest-path")
        .arg(format!("{dir}/Cargo.toml"))
        .arg("--")
        .arg(format!("{dir}/../{path}"))
        .arg("-L")
        .arg(lib_dir)
        .arg("--crate-type")
        .arg("rlib");
    let status = cmd.status().unwrap();
    assert!(
        status.success(),
        "{path:?}: c2rust-analyze failed with status {status:?}",
    );
}
