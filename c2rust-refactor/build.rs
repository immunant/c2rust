use c2rust_build_paths::SysRoot;
use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn process_ast(mode: &str, dest: &Path) {
    let mut p = Command::new("python3")
        .arg("-B") // Don't write bytecode files (and thus pollute the source
        // directory)
        .arg("gen/process_ast.py")
        .arg(mode)
        .arg(dest)
        .spawn()
        .expect("failed to run process_ast.py. Make sure python3 is in your PATH.");

    let ret = p.wait().expect("failed to wait on process_ast.py");

    assert!(ret.success(), "error while running process_ast.py");
}

fn main() {
    let out_dir_str = env::var("OUT_DIR").unwrap();
    let out_dir = Path::new(&out_dir_str);

    process_ast("ast_deref", &out_dir.join("ast_deref_gen.inc.rs"));
    process_ast("ast_equiv", &out_dir.join("ast_equiv_gen.inc.rs"));
    process_ast("matcher", &out_dir.join("matcher_impls_gen.inc.rs"));
    process_ast("get_span", &out_dir.join("get_span_gen.inc.rs"));
    process_ast("get_node_id", &out_dir.join("get_node_id_gen.inc.rs"));
    process_ast("lr_expr", &out_dir.join("lr_expr_gen.inc.rs"));
    process_ast("list_node_ids", &out_dir.join("list_node_ids_gen.inc.rs"));
    process_ast("mac_table", &out_dir.join("mac_table_gen.inc.rs"));
    process_ast("nt_match", &out_dir.join("nt_match_gen.inc.rs"));
    process_ast("ast_names", &out_dir.join("ast_names_gen.inc.rs"));

    process_ast(
        "rewrite_rewrite",
        &out_dir.join("rewrite_rewrite_gen.inc.rs"),
    );
    process_ast(
        "rewrite_recursive",
        &out_dir.join("rewrite_recursive_gen.inc.rs"),
    );
    process_ast(
        "rewrite_recover_children",
        &out_dir.join("rewrite_recover_children_gen.inc.rs"),
    );
    process_ast(
        "rewrite_seq_item",
        &out_dir.join("rewrite_seq_item_gen.inc.rs"),
    );
    process_ast(
        "rewrite_maybe_rewrite_seq",
        &out_dir.join("rewrite_maybe_rewrite_seq_gen.inc.rs"),
    );

    println!("cargo:rerun-if-changed=gen/");
    for entry in fs::read_dir(&"gen").unwrap() {
        let name = entry.unwrap().file_name().into_string().unwrap();
        if name.starts_with(".") {
            continue;
        }
        println!("cargo:rerun-if-changed=gen/{}", name);
    }

    let sysroot = SysRoot::resolve();
    sysroot.link_rustc_private();
}
