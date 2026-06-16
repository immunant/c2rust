use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn temp_dir() -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let path = std::env::temp_dir().join(format!(
        "merge_rust_reject_invalid_{}_{}",
        std::process::id(),
        nanos
    ));
    fs::create_dir(&path).unwrap();
    path
}

#[test]
fn update_only_rejects_invalid_replacement_without_writing() {
    let dir = temp_dir();
    let src_path = dir.join("lib.rs");
    let snippets_path = dir.join("snippets.json");
    let original = "\
pub fn good() -> i32 {
    1
}

pub fn other() -> i32 {
    2
}
";

    fs::write(&src_path, original).unwrap();
    fs::write(
        &snippets_path,
        "{\"good\":\"pub fn good() -> i32 {\\n    1\\n\"}",
    )
    .unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_merge_rust"))
        .arg("--update-only")
        .arg(&src_path)
        .arg(&snippets_path)
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_eq!(fs::read_to_string(&src_path).unwrap(), original);

    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains("merged Rust in"));
    assert!(stderr.contains("failed to parse"));

    fs::remove_dir_all(dir).unwrap();
}
