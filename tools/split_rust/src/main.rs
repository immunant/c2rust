use clap::Parser;
use rust_util::collect::FileCollector;
use rust_util::item_span::item_spans;
use serde_json;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

/// Split a Rust codebase into a JSON map from item paths to their source text.
#[derive(Parser)]
struct Args {
    /// Root Rust source file to split (`lib.rs` or `main.rs`).
    src_root_path: PathBuf,
}

fn main() {
    let args = Args::parse();
    let mut fc = FileCollector::default();
    fc.parse(args.src_root_path, vec![], true).unwrap();
    let mut out = HashMap::new();
    for &(ref name, ref mod_path, ref ast) in &fc.files {
        eprintln!("visit {:?}", name);
        let src = fs::read_to_string(name).unwrap();
        for (item_path, lo, hi) in item_spans(mod_path.to_owned(), ast) {
            let snippet = &src[lo..hi];
            out.insert(item_path.join("::"), snippet.to_owned());
        }
    }
    serde_json::to_writer(std::io::stdout(), &out).unwrap();
}
