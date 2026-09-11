pub mod common;

use crate::common::check_for_missing_tests_for;
use crate::common::test_dir_for;
use crate::common::Analyze;
use crate::common::CrateOptions;
use crate::common::CrateType;
use fs_err::File;
use std::path::Path;
use std::process::Command;

#[test]
fn check_for_missing_tests() {
    check_for_missing_tests_for(file!());
}

fn test(file_name: &str) {
    let analyze = Analyze::resolve();
    let path = test_dir_for(file!(), true).join(file_name);
    analyze.run(&path);
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
    macros,
    ptr_addr_of,
    rust_intrinsic,
    string_literals,
    string_casts,
}

#[test]
fn lighttpd_minimal() {
    let analyze = Analyze::resolve();
    let mut cmd = Command::new(analyze.path());

    cmd.arg("--");

    cmd.arg("check");

    let dir = Path::new("../analysis/tests/lighttpd-minimal");
    let manifest_path = dir.join("Cargo.toml");
    cmd.arg("--manifest-path").arg(manifest_path);

    let output_path = dir.join("analysis.txt");
    let output_stdout = File::create(&output_path).unwrap();
    let output_stderr = File::try_clone(&output_stdout).unwrap();
    cmd.stdout(output_stdout.into_parts().0)
        .stderr(output_stderr.into_parts().0);

    let status = cmd.status().unwrap();
    assert!(status.success());

    // TODO(kkysen) Handle error reporting better like [`Analyze::run`].
}

#[test]
fn with_pdg_file() {
    use std::path::PathBuf;
    let pdg_path: PathBuf = "../analysis/tests/minimal/reference_pdg.bc".into();
    println!("{:?}", std::env::current_dir());
    let pdg_path = pdg_path.canonicalize().unwrap();
    let crate_options = CrateOptions {
        crate_type: CrateType::Bin,
        ..Default::default()
    };
    Analyze::resolve().run_with(
        "../analysis/tests/minimal/src/main.rs",
        |cmd| {
            cmd.env("PDG_FILE", &pdg_path).args(&[
                "--crate-name",
                "c2rust_analysis_tests_minimal",
                "-C",
                "metadata=4095517b1921578c",
                "-C",
                "extra-filename=-4095517b1921578c",
            ]);
        },
        Some(crate_options),
    );
}

#[test]
fn mir_lowering() {
    let analyze = Analyze::resolve();
    let fixture = test_dir_for(file!(), true).join("mir_lowering.rs");
    let test_dir = std::env::temp_dir().join(format!(
        "c2rust-analyze-mir-lowering-{}",
        std::process::id()
    ));
    for edition in [2021, 2024] {
        let dir = test_dir.join(edition.to_string());
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("mir_lowering.rs");
        std::fs::copy(&fixture, &path).unwrap();
        let output = analyze.run_with(
            &path,
            |cmd| {
                cmd.env("C2RUST_ANALYZE_REWRITE_MODE", "inplace")
                    .arg("--out-dir")
                    .arg(&dir);
            },
            Some(CrateOptions {
                edition,
                crate_type: CrateType::Bin,
            }),
        );
        let diagnostics = std::fs::read_to_string(output).unwrap();
        assert!(!diagnostics.contains("[ERROR"), "{diagnostics}");
        let rewritten = std::fs::read_to_string(&path).unwrap();
        let code = rewritten
            .lines()
            .filter(|line| !line.trim_start().starts_with("//"))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(!code.contains("p: *"), "{rewritten}");
        assert!(code.contains("let element: &(i32)"), "{rewritten}");
        assert!(
            code.contains("p: core::option::Option<&'h0 (i32)>"),
            "{rewritten}"
        );
        assert!(code.contains("p: &'h0 (i32)"), "{rewritten}");
        assert!(code.contains(".is_none()"), "{rewritten}");
        assert!(!code.contains("p as *const"), "{rewritten}");
        let binary = dir.join("mir_lowering");
        let compile = Command::new(
            c2rust_build_paths::SysRoot::resolve()
                .sysroot()
                .join("bin/rustc"),
        )
        .arg(&path)
        .arg("--edition")
        .arg(edition.to_string())
        .arg("-o")
        .arg(&binary)
        .output()
        .unwrap();
        assert!(
            compile.status.success(),
            "{}\n{rewritten}",
            String::from_utf8_lossy(&compile.stderr)
        );
        assert!(Command::new(binary).status().unwrap().success());
    }
    std::fs::remove_dir_all(test_dir).unwrap();
}
