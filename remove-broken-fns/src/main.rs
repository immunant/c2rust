use anyhow::Result;
use core::ops::Range;
use json;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use syn::spanned::Spanned;

/// Return start offset of every line in input
fn line_offsets(s: &str) -> Vec<usize> {
    let mut line_starts = vec![0];
    let mut remaining_input = &s[..];
    let mut last_line_start = 0usize;
    while let Some(n) = remaining_input.find("\n") {
        last_line_start += n + 1;
        assert!(s.as_bytes()[last_line_start - 1] == b'\n');
        line_starts.push(last_line_start);
        remaining_input = &remaining_input[n + 1..];
    }
    line_starts
}

/// Convert a `proc_macro2::LineColumn` to a linear byte offset
fn line_column_to_offset(lc: proc_macro2::LineColumn, input: &str, line_starts: &[usize]) -> usize {
    let line_start = line_starts[lc.line - 1];

    let line_text = &input[line_start..];
    let (offset_in_line, _) = line_text
        .char_indices()
        .chain(Some((line_text.len(), '\n')))
        .skip(lc.column)
        .next()
        .expect(&format!(
            "ran out of chars skipping to {} in string: {}",
            lc.column, line_text
        ));

    line_start + offset_in_line
}

/// Find byte offset bounds of each fn item in the file
fn parse_fn_bounds(contents: &syn::File, contents_str: &str) -> Vec<(String, Range<usize>)> {
    let line_starts = line_offsets(contents_str);
    let to_offset = move |lc| line_column_to_offset(lc, contents_str, &line_starts);

    let mut fn_bounds = vec![];
    for item in &contents.items {
        if let syn::Item::Fn(fn_item) = item {
            let span = fn_item.span();
            let start = to_offset(span.start());
            let end = to_offset(span.end());
            fn_bounds.push((fn_item.sig.ident.to_string(), start..end));
        }
    }
    fn_bounds
}

/// Extract error locations from JSON error output
fn error_locations(rustc_stderr_json: &str) -> Result<Vec<(String, usize)>> {
    let mut error_locs = vec![];
    for line in rustc_stderr_json.lines() {
        let line = json::parse(line)?;
        // Skip non-error messages and those without spans
        if line["level"] != "error" || line["spans"].len() == 0 {
            continue;
        }
        // Only primary spans should count as error sources
        for span in line["spans"].members().filter(|span| {
            span["is_primary"]
                .as_bool()
                .expect(&format!(".is_primary should be a bool: {:?}", span))
        }) {
            error_locs.push((
                span["file_name"]
                    .as_str()
                    .expect(".file_name should be a string")
                    .to_owned(),
                span["byte_start"]
                    .as_usize()
                    .expect(".byte_start should be an int"),
            ));
        }
    }
    Ok(error_locs)
}

/// Join across error locations and fn item bounds
fn fns_to_remove(
    error_locs: &[(String, usize)],
) -> Result<std::collections::HashSet<(String, Range<usize>)>> {
    let mut fns_to_remove = std::collections::HashSet::<_>::new();
    let mut fn_bounds_for_file = std::collections::HashMap::<String, _>::new();

    for (filename, byte_start) in error_locs {
        let bounds = fn_bounds_for_file
            .entry(filename.clone())
            .or_insert(move || {
                let contents = std::fs::read_to_string(&filename).unwrap();
                let syntax_tree = syn::parse_file(&contents).unwrap();
                parse_fn_bounds(&syntax_tree, &contents)
            })();
        for func in bounds {
            let (ref _fn_name, ref fn_bounds) = func;
            if fn_bounds.contains(&byte_start) {
                fns_to_remove.insert(func);
            }
        }
    }
    Ok(fns_to_remove)
}

fn main() -> Result<()> {
    let input_path: PathBuf = std::env::args_os()
        .nth(1)
        .expect("usage: remove-broken-fns <file.rs>")
        .into();

    // Run rustc
    let rustc_output = Command::new("rustc")
        .arg("--error-format=json")
        .arg(&input_path)
        .stdin(Stdio::null())
        .output()?;

    if rustc_output.status.success() {
        return Ok(());
    }
    let stderr = String::from_utf8(rustc_output.stderr)?;

    let error_locs = error_locations(&stderr)?;
    let fns_to_remove = fns_to_remove(&error_locs)?;

    println!("fn to remove: {:?}", fns_to_remove);

    Ok(())
}
