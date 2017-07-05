use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn process_ast(mode: &str, dest: &Path) {
    let mut p = Command::new("python3")
        .arg("gen/process_ast.py")
        .arg(mode)
        .arg(dest)
        .spawn()
        .expect("failed to run process_ast.py");

    let ret = p.wait().expect("failed to wait on process_ast.py");

    assert!(ret.success(), "error while running process_ast.py");
}

fn main() {
    let out_dir_str = env::var("OUT_DIR").unwrap();
    let out_dir = Path::new(&out_dir_str);

    process_ast("ast_equiv", &out_dir.join("ast_equiv_gen.inc.rs"));
    process_ast("matcher", &out_dir.join("matcher_impls_gen.inc.rs"));
    process_ast("rewrite", &out_dir.join("rewrite_impls_gen.inc.rs"));
    process_ast("get_span", &out_dir.join("get_span_gen.inc.rs"));
    process_ast("get_node_id", &out_dir.join("get_node_id_gen.inc.rs"));

    println!("cargo:rerun-if-changed=gen/");
    for entry in fs::read_dir(&"gen").unwrap() {
        let name = entry.unwrap().file_name().into_string().unwrap();
        if name.starts_with(".") {
            continue;
        }
        println!("cargo:rerun-if-changed=gen/{}", name);
    }
}
