use clap::Parser;
use indexmap::IndexMap;
use rust_util::collect::FileCollector;
use rust_util::item_span::item_spans;
use serde_json;
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::iter;
use std::path::{Path, PathBuf};
use syn;

/// Merge updated item definitions into a Rust codebase.
#[derive(Parser)]
struct Args {
    /// Root Rust source file to update (`lib.rs` or `main.rs`).
    src_root_path: PathBuf,
    /// JSON file containing mapping from Rust item paths to desired new contents.
    new_snippets_file: PathBuf,

    /// Use the JSON contents to overwrite existing definitions, but don't add or remove anything.
    #[clap(long)]
    update_only: bool,
}

type ModPath = String;

fn main() {
    let args = Args::parse();
    let src_root_path = args.src_root_path;
    let src_root_dir = Path::new(&src_root_path).parent().unwrap();
    let new_snippet_json_path = args.new_snippets_file;

    let new_snippets_file = File::open(&new_snippet_json_path).unwrap();
    let new_snippets: IndexMap<String, String> =
        serde_json::from_reader(new_snippets_file).unwrap();

    let mut fc = FileCollector::default();
    fc.parse(&src_root_path, vec![], true).unwrap();

    let mut files: Vec<(PathBuf, ModPath, syn::File)> = fc.files.into_iter()
        .map(|(file_path, mod_path_parts, ast)| (file_path, mod_path_parts.join("::"), ast))
        .collect();
    // Gives the file path and end position for each module.
    let mut mod_locations = fc.mods.iter().map(|m| {
        (m.mod_path.join("::"), (m.file_path.clone(), m.inner_end_pos))
    }).collect::<HashMap<ModPath, (PathBuf, usize)>>();


    // For every module mentioned in `new_snippets`, if the module doesn't exist in `fc.mods`,
    // create it.
    let snippet_modules = new_snippets.keys()
        .map(|k| k.rsplit_once("::").map_or("", |(parent_path, _child_name)| parent_path))
        .map(|x| x.to_owned())
        .collect::<HashSet<_>>();
    let mut new_snippets = new_snippets;
    for mod_path in &snippet_modules {
        // Iterate over all ancestors of `mod_path`.
        let idxs = iter::once(mod_path.len())
            .chain(mod_path.rmatch_indices("::").map(|(idx, _)| idx));
        for idx in idxs {
            let mod_path = &mod_path[..idx];
            // Skip modules that already exist.
            if mod_locations.contains_key(mod_path) {
                continue;
            }

            // Create an empty file on disk and add it to `files`.
            debug_assert!(mod_path.len() != 0);
            let file_path_rel = mod_path.replace("::", "/") + ".rs";
            let file_path = src_root_dir.join(&file_path_rel);
            assert!(!fs::exists(&file_path).unwrap(),
                "file {:?} is missing from mod_spans, but exists on disk?", file_path);
            fs::write(&file_path, "").unwrap();
            let ast = syn::File {
                shebang: None,
                attrs: Vec::new(),
                items: Vec::new(),
            };
            files.push((file_path.clone(), mod_path.to_owned(), ast));
            // The file is empty, so new items should be inserted at byte position 0.
            mod_locations.insert(mod_path.to_owned(), (file_path, 0));

            // Add a `mod foo;` snippet to the parent module.
            let (_parent_mod_path, mod_name) = mod_path.rsplit_once("::")
                .unwrap_or(("", mod_path));
            let old = new_snippets.insert(mod_path.to_owned(), format!("mod {mod_name};"));
            assert!(old.is_none(), "item {:?} exists but is not a module", mod_path);
        }
    }
    let new_snippets = new_snippets;


    let mut file_rewrites = IndexMap::<PathBuf, Vec<(usize, usize, &str)>>::new();

    // Collect rewrites for updated or removed items.  We record each item in `snippets_applied` as
    // we apply it.
    let mut snippets_applied = HashSet::<String>::new();
    for &(ref file_path, ref mod_path, ref ast) in &files {
        eprintln!("visit {file_path:?}");
        let old_src = fs::read_to_string(file_path).unwrap();

        let rewrites = file_rewrites.entry(file_path.to_owned()).or_insert(Vec::new());

        // Update or remove existing items.
        let mod_path_parts = if mod_path == "" {
            Vec::new()
        } else {
            mod_path.split("::").map(|s| s.to_owned()).collect::<Vec<String>>()
        };
        for (item_path, lo, hi) in item_spans(mod_path_parts, ast) {
            let old_snippet = &old_src[lo..hi];
            let item_path_str = item_path.join("::");
            let new_snippet = match new_snippets.get(&item_path_str) {
                Some(x) => {
                    snippets_applied.insert(item_path_str);
                    x
                },
                None => {
                    if args.update_only {
                        // We would normally delete this item, but we're currently in
                        // update-only mode.
                        continue;
                    } else {
                        ""
                    }
                },
            };
            if new_snippet != old_snippet {
                rewrites.push((lo, hi, new_snippet));
            }
        }
    }

    // Collect rewrites for newly added items.  Any entry in `new_snippets` that wasn't added to
    // `snippets_applied` above must be a newly added item.
    if !args.update_only {
        for (item_path, new_snippet) in &new_snippets {
            if snippets_applied.contains(item_path) {
                continue;
            }
            let mod_path = item_path.rsplit_once("::").map_or("", |(parent, _child)| parent);
            let &(ref file_path, end_pos) = mod_locations.get(mod_path).unwrap_or_else(|| {
                unreachable!("parent mod for {:?} should be added above", item_path);
            });
            let rewrites = file_rewrites.entry(file_path.clone()).or_insert(Vec::new());
            rewrites.push((end_pos, end_pos, "\n\n"));
            rewrites.push((end_pos, end_pos, new_snippet));
        }
    }

    // Apply the collected rewrites to each file.
    for (file_path, mut rewrites) in file_rewrites {
        if rewrites.len() == 0 {
            continue;
        }

        let old_src = fs::read_to_string(&file_path).unwrap();

        // Apply rewrites
        //
        // Sort by `lo`, then by `hi`.  This means `0..10 < 10..10 < 10..20`.
        rewrites.sort_by_key(|&(lo, hi, _)| (lo, hi));
        let mut new_src = String::with_capacity(old_src.len());
        let mut pos = 0;
        for &(lo, hi, ref new_snippet) in &rewrites {
            assert!(
                lo >= pos,
                "overlapping rewrites: previous rewrite ended at {}, \
                but current rewrite covers {} .. {}",
                pos,
                lo,
                hi
            );
            new_src.push_str(&old_src[pos..lo]);
            new_src.push_str(&new_snippet);
            pos = hi;
        }
        new_src.push_str(&old_src[pos..]);

        let tmp_path = file_path.with_extension(".new");
        fs::write(&tmp_path, &new_src).unwrap();
        fs::rename(&tmp_path, &file_path).unwrap();
        eprintln!("applied {} rewrites to {:?}", rewrites.len(), file_path);
    }
}
