pub mod common;

use std::fs;

use common::{Analyze, FileCheck};

#[test]
fn filecheck() {
    let analyze = Analyze::new();
    let file_check = FileCheck::resolve();

    for entry in fs::read_dir("tests/filecheck").unwrap() {
        let entry = entry.unwrap();

        if !entry.file_type().unwrap().is_file() {
            continue;
        }

        let name = entry.file_name();
        let name = name.to_str().unwrap();
        if name.starts_with('.') || !name.ends_with(".rs") {
            continue;
        }

        eprintln!("{:?}", entry.path());

        let output_path = analyze.run(entry.path());
        file_check.run(entry.path(), &output_path);
    }
}
