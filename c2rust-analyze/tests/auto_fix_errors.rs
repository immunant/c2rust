pub mod common;

use crate::common::{check_for_missing_tests_for, test_dir_for};
use std::fs::{self, File};
use std::process::Command;

#[test]
fn check_for_missing_tests() {
    check_for_missing_tests_for(file!());
}

fn test(file_name: &str) {
    let test_dir = test_dir_for(file!(), true);
    let path = test_dir.join(file_name);
    let fixed_path = path.with_extension("rs.fixed");
    let json_path = path.with_extension("json");
    let script_path = test_dir.join("../../scripts/auto_fix_errors.py");

    fs::copy(&path, &fixed_path).unwrap();

    // Run with `--error-format json` to produce JSON input for the script.
    let mut cmd = Command::new("rustc");
    cmd.arg("-A")
        .arg("warnings")
        .arg("--crate-name")
        .arg(path.file_stem().unwrap())
        .arg("--crate-type")
        .arg("rlib")
        .arg("--error-format")
        .arg("json")
        .arg(&fixed_path)
        .stderr(File::create(&json_path).unwrap());
    let status = cmd.status().unwrap();
    assert_eq!(
        status.code(),
        Some(1),
        "command {cmd:?} exited with code {status:?}"
    );

    // Run the script to fix errors.
    let mut cmd = Command::new(script_path);
    cmd.arg(&json_path);
    let status = cmd.status().unwrap();
    assert!(
        status.success(),
        "command {cmd:?} exited with code {status:?}"
    );

    // There should be no more compile errors now.
    let mut cmd = Command::new("rustc");
    cmd.arg("-A")
        .arg("warnings")
        .arg("--crate-name")
        .arg(path.file_stem().unwrap())
        .arg("--crate-type")
        .arg("rlib")
        .arg(&fixed_path);
    let status = cmd.status().unwrap();
    assert!(
        status.success(),
        "command {cmd:?} exited with code {status:?}"
    );
}

macro_rules! define_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            test(concat!(stringify!($name), ".rs"));
        }
    };
}

macro_rules! define_tests {
    ($($name:ident,)*) => {
        $(define_test! { $name })*
    }
}

define_tests! {
    derive_copy,
}
