use std::env;
use std::fs;
use std::os::unix::io::{AsRawFd, FromRawFd};
use std::process::{Command, Stdio};

fn detect_filecheck() -> Option<&'static str> {
    let candidates = [
        "FileCheck",
        "/usr/local/opt/llvm/bin/FileCheck",
        "FileCheck-14",
        "/usr/local/opt/llvm@14/bin/FileCheck",
        "FileCheck-13",
        "/usr/local/opt/llvm@13/bin/FileCheck",
        "FileCheck-12",
        "/usr/local/opt/llvm@12/bin/FileCheck",
        "FileCheck-11",
        "/usr/local/opt/llvm@11/bin/FileCheck",
        "FileCheck-10",
        "/usr/local/opt/llvm@10/bin/FileCheck",
        "FileCheck-9",
        "/usr/local/opt/llvm@9/bin/FileCheck",
        "FileCheck-8",
        "/usr/local/opt/llvm@8/bin/FileCheck",
        "FileCheck-7",
        "FileCheck-7.0",
        "/usr/local/opt/llvm@7/bin/FileCheck",
    ];

    for filecheck in candidates {
        let result = Command::new(filecheck)
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status();
        if result.is_ok() {
            return Some(filecheck);
        }
    }
    None
}

#[test]
fn filecheck() {
    let lib_dir = env::var("C2RUST_TARGET_LIB_DIR").unwrap();
    let lib_dir = &lib_dir;

    let filecheck_bin = env::var("FILECHECK")
        .ok()
        .or_else(|| detect_filecheck().map(|s| s.to_owned()))
        .unwrap_or_else(|| panic!("FileCheck not found - set FILECHECK=/path/to/FileCheck"));
    eprintln!("detected FILECHECK={}", filecheck_bin);

    for entry in fs::read_dir("tests/filecheck").unwrap() {
        let entry = entry.unwrap();

        if !entry.file_type().unwrap().is_file() {
            continue;
        }

        let name = entry.file_name();
        let name = name.to_str().unwrap();
        if name.starts_with(".") || !name.ends_with(".rs") {
            continue;
        }

        eprintln!("{:?}", entry.path());

        let mut filecheck_cmd = Command::new(&filecheck_bin);
        filecheck_cmd.arg(entry.path()).stdin(Stdio::piped());
        let mut filecheck = filecheck_cmd.spawn().unwrap();
        let pipe_fd = filecheck.stdin.as_ref().unwrap().as_raw_fd();
        let mut analyze_cmd = Command::new("cargo");
        analyze_cmd
            .arg("run")
            .arg("--manifest-path")
            .arg(format!("{}/Cargo.toml", env!("CARGO_MANIFEST_DIR")))
            .arg("--")
            .arg(entry.path())
            .arg("-L")
            .arg(lib_dir)
            .arg("--crate-type")
            .arg("rlib")
            .stdout(unsafe { Stdio::from_raw_fd(pipe_fd) })
            .stderr(unsafe { Stdio::from_raw_fd(pipe_fd) });
        let mut analyze = analyze_cmd.spawn().unwrap();

        let filecheck_status = filecheck.wait().unwrap();
        assert!(
            filecheck_status.success(),
            "{:?}: FileCheck failed with status {:?}",
            entry.path(),
            filecheck_status,
        );

        let analyze_status = analyze.wait().unwrap();
        assert!(
            analyze_status.success(),
            "{:?}: c2rust-analyze failed with status {:?}",
            entry.path(),
            analyze_status,
        );
    }
}
