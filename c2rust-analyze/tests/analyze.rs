pub mod common;

use crate::common::{check_for_missing_tests_for, test_dir_for, Analyze, CrateOptions, CrateType};

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
    string_literals,
    string_casts,
}

#[test]
fn lighttpd_minimal() {
    Analyze::resolve().run("../analysis/tests/lighttpd-minimal/src/main.rs");
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
