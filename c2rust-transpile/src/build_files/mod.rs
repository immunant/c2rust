extern crate handlebars;

use std::env;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::io::Write;

use self::handlebars::Handlebars;
use super::serde_json::{json};

use super::TranspilerConfig;

pub fn emit_build_files(tcfg: &TranspilerConfig, cc_db: &Path, modules: Vec<PathBuf>) {

    let build_dir = cc_db
        .parent() // get directory of `compile_commands.json`
        .unwrap()
        .join("c2rust-build");

    let mut reg = Handlebars::new();

    reg.register_template_string("Cargo.toml", include_str!("Cargo.toml.hbs")).unwrap();
    reg.register_template_string("lib.rs", include_str!("lib.rs.hbs")).unwrap();

    emit_cargo_toml(tcfg,&reg, &build_dir);
    emit_lib_rs(tcfg, &reg, &build_dir);
}

/// Emit `lib.rs` for libraries and `main.rs` for binaries.
fn emit_lib_rs(tcfg: &TranspilerConfig, reg: &Handlebars, build_dir: &Path) {
    let plugin_args = tcfg.cross_check_configs
        .iter()
        .map(|ccc| format!("config_file = \"{}\"", ccc))
        .collect::<Vec<String>>()
        .join(", ");
    let json = json!({
        "reorganize_definitions": tcfg.reorganize_definitions,
        "cross_checks": tcfg.cross_checks,
        "use_fakechecks": tcfg.use_fakechecks,
        "main_module": tcfg.main,
        "plugin_args": plugin_args
    });

    let file_name = match tcfg.main { Some(_) => "main.rs", None => "lib.rs" };
    let output_path = build_dir.join(file_name);
    let output = reg.render("lib.rs", &json).unwrap();
//    println!("{}", output);

    write_to_file(&output_path, output);
}

fn emit_cargo_toml(tcfg: &TranspilerConfig, reg: &Handlebars, build_dir: &Path) {
    // TODO: get C2RUST_HOME from environment variable
    let rust_checks_path = env::current_dir()
        .unwrap()
        // Assumes `current_dir` is $C2RUST_HOME/c2rust
        .join("../cross-checks/rust-checks")
        .canonicalize()
        .unwrap();
    let plugin_path = rust_checks_path.join("rustc-plugin");
    let derive_path = rust_checks_path.join("derive-macros");
    let runtime_path = rust_checks_path.join("runtime");
    let libfakechecks_sys_path = rust_checks_path.join("backends/libfakechecks-sys");
    let json = json!({
        "crate_name": "c2rust-build",
        "main_module": tcfg.main,
        "cross_checks": tcfg.cross_checks,
        "use_fakechecks": tcfg.use_fakechecks,
        "plugin_path": plugin_path,
        "derive_path": derive_path,
        "runtime_path": runtime_path,
        "libfakechecks_sys_path": libfakechecks_sys_path
    });
    let file_name = "Cargo.toml";
    let output_path = build_dir.join(file_name);
    let output = reg.render(file_name, &json).unwrap();
//    println!("{}", output);
    write_to_file(&output_path, output);
}

fn write_to_file(output_path: &Path, output: String) {
    let mut file = match File::create(&output_path) {
        Ok(file) => file,
        Err(e) => panic!("Unable to open file for writing: {}", e),
    };
    match file.write_all(output.as_bytes()) {
        Ok(()) => (),
        Err(e) => panic!("Unable to write translation to file: {}", e),
    };
}