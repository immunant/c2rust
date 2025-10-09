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
