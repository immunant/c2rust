use c2rust_ast_exporter::clang_ast::{AstContext, Value};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

fn sentinel_present(ast: &AstContext) -> bool {
    ast.ast_nodes.values().any(|node| {
        node.extras
            .iter()
            .any(|extra| matches!(extra, Value::Text(text) if text == "c2rust_sentinel_ptrdiff_t"))
    })
}

fn write_probe(root: &Path) -> (PathBuf, PathBuf) {
    let include = root.join("include");
    fs::create_dir_all(&include).unwrap();
    fs::write(
        include.join("stddef.h"),
        "typedef long c2rust_sentinel_ptrdiff_t;\n",
    )
    .unwrap();

    let probe = root.join("probe.c");
    fs::write(
        &probe,
        "#include <stddef.h>\nc2rust_sentinel_ptrdiff_t c2rust_probe;\n",
    )
    .unwrap();

    let directory = root
        .to_str()
        .unwrap()
        .replace('\\', "\\\\")
        .replace('"', "\\\"");
    let cc_db = root.join("compile_commands.json");
    fs::write(
        &cc_db,
        format!(
            "[\n  {{\n    \"directory\": \"{directory}\",\n    \"command\": \"cc -c probe.c\",\n    \"file\": \"probe.c\"\n  }}\n]\n"
        ),
    )
    .unwrap();

    (probe, cc_db)
}

/// Removes the temporary tree when dropped, including after an assertion
/// failure, and reports a removal error with the path that remains.
struct RemoveTempDir(PathBuf);

impl Drop for RemoveTempDir {
    fn drop(&mut self) {
        if let Err(err) = fs::remove_dir_all(&self.0) {
            if err.kind() == std::io::ErrorKind::NotFound {
                return;
            }
            let message = format!(
                "failed to remove temporary directory {}: {err}",
                self.0.display()
            );
            if std::thread::panicking() {
                eprintln!("{message}");
            } else {
                panic!("{message}");
            }
        }
    }
}

#[test]
fn resource_dir_finds_sentinel_stddef() {
    let unique = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let root = env::temp_dir().join(format!("c2rust-resource-dir-{unique}"));
    let _cleanup = RemoveTempDir(root.clone());
    let (probe, cc_db) = write_probe(&root);
    let resource_arg = format!("-resource-dir={}", root.to_str().unwrap());

    let (ast, _) =
        c2rust_ast_exporter::get_untyped_ast(&probe, &cc_db, &[&resource_arg], false, false)
            .expect("case A: exporter failed");
    assert!(
        sentinel_present(&ast),
        "case A: sentinel typedef missing from AST"
    );

    let previous = env::var("C2RUST_CLANG_RESOURCE_DIR").ok();
    env::set_var("C2RUST_CLANG_RESOURCE_DIR", &root);
    let case_b = c2rust_ast_exporter::get_untyped_ast(&probe, &cc_db, &[], false, false);
    match &previous {
        Some(value) => env::set_var("C2RUST_CLANG_RESOURCE_DIR", value),
        None => env::remove_var("C2RUST_CLANG_RESOURCE_DIR"),
    }
    let (ast, _) = case_b.expect("case B: exporter failed");
    assert!(
        sentinel_present(&ast),
        "case B: sentinel typedef missing from AST"
    );
}
