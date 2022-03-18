#![feature(rustc_private)]

extern crate rustc_ast_pretty;
extern crate rustc_parse;
extern crate rustc_session;
extern crate rustc_span;

use anyhow::Result;
use quote::quote;
use rustc_session::parse::ParseSess;
use rustc_span::edition::Edition::Edition2021;
use rustc_span::source_map::FilePathMapping;
use std::fs::{self, File};
use std::path::Path;
use std::process::{Command, Stdio};

fn main() -> Result<()> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));

    // Read and parse input.rs
    let input_path = manifest_dir.join("..").join("input.rs");
    let input_contents = fs::read_to_string(&input_path)?;
    let syntax_tree = syn::parse_file(&input_contents)?;

    // Write input.rs
    let tokens = quote!(#syntax_tree);
    let mut string = tokens.to_string();
    string.push('\n');
    fs::write(&input_path, string)?;

    // Write output.prettyplease.rs
    let output_path = manifest_dir.join("..").join("output.prettyplease.rs");
    let string = prettyplease::unparse(&syntax_tree);
    fs::write(&output_path, string)?;

    // Write output.rustc.rs
    let output_path = manifest_dir.join("..").join("output.rustc.rs");
    let mut string = rustc_span::create_session_globals_then(Edition2021, || {
        let sess = ParseSess::new(FilePathMapping::new(Vec::new()));
        let krate = rustc_parse::parse_crate_from_file(&input_path, &sess).unwrap();
        rustc_ast_pretty::pprust::crate_to_string_for_macros(&krate)
    });
    string.push('\n');
    fs::write(&output_path, string)?;

    // Write output.rustfmt.rs
    let output_path = manifest_dir.join("..").join("output.rustfmt.rs");
    let output_file = File::create(output_path)?;
    Command::new("rustfmt")
        .arg("--edition=2021")
        .arg("--config=reorder_imports=false")
        .arg("--config=normalize_doc_attributes=true")
        .arg("--emit=stdout")
        .arg("--quiet")
        .arg("--unstable-features")
        .arg("--skip-children")
        .arg(&input_path)
        .stdin(Stdio::null())
        .stdout(output_file)
        .stderr(Stdio::inherit())
        .spawn()?
        .wait()?;

    Ok(())
}
