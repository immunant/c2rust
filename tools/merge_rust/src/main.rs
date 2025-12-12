use clap::Parser;
use rust_util::collect::FileCollector;
use rust_util::item_span::item_spans;
use serde_json;
use std::collections::HashMap;
use std::fs::{self, File};
use std::path::PathBuf;

/// Merge updated item definitions into a Rust codebase.
#[derive(Parser)]
struct Args {
    /// Root Rust source file to update (`lib.rs` or `main.rs`).
    src_root_path: PathBuf,
    /// JSON file containing mapping from Rust item paths to desired new contents.
    new_snippets_file: PathBuf,
}

fn main() {
    let args = Args::parse();
    let src_root_path = args.src_root_path;
    let new_snippet_json_path = args.new_snippets_file;

    let new_snippets_file = File::open(&new_snippet_json_path).unwrap();
    let new_snippets: HashMap<String, String> = serde_json::from_reader(new_snippets_file).unwrap();

    let mut fc = FileCollector::default();
    fc.parse(&src_root_path, vec![], true).unwrap();
    for &(ref file_path, ref mod_path, ref ast) in &fc.files {
        eprintln!("visit {:?}", file_path);
        let old_src = fs::read_to_string(file_path).unwrap();

        let mut rewrites = Vec::new();
        for (item_path, lo, hi) in item_spans(mod_path.to_owned(), ast) {
            let old_snippet = &old_src[lo..hi];
            let item_path_str = item_path.join("::");
            if let Some(new_snippet) = new_snippets.get(&item_path_str) {
                if new_snippet != old_snippet {
                    rewrites.push((lo, hi, new_snippet));
                }
            }
        }

        if rewrites.len() == 0 {
            continue;
        }

        rewrites.sort_by_key(|&(lo, _, _)| lo);
        let mut new_src = String::with_capacity(old_src.len());
        let mut pos = 0;
        for &(lo, hi, new_snippet) in &rewrites {
            assert!(
                lo >= pos,
                "overlapping rewrites: previous rewrite ended at {}, \
                but current rewrite covers {} .. {}",
                pos,
                lo,
                hi
            );
            new_src.push_str(&old_src[pos..lo]);
            new_src.push_str(new_snippet);
            pos = hi;
        }
        new_src.push_str(&old_src[pos..]);

        let tmp_path = file_path.with_extension(".new");
        fs::write(&tmp_path, &new_src).unwrap();
        fs::rename(&tmp_path, file_path).unwrap();
        eprintln!("applied {} rewrites to {:?}", rewrites.len(), file_path);
    }
}
