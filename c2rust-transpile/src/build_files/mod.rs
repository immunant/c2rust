extern crate handlebars;

use std::env;
use std::fs::File;
use std::fs::DirBuilder;
use std::path::{Path, PathBuf};
use std::io::Write;

use self::handlebars::Handlebars;
use super::serde_json::{json};

use super::TranspilerConfig;

pub fn emit_build_files(tcfg: &TranspilerConfig, cc_db: &Path,
                        modules: Vec<PathBuf>) {

    let build_dir = cc_db
        .parent() // get directory of `compile_commands.json`
        .unwrap()
        .join("c2rust-build");
    if !build_dir.exists() {
        let db = DirBuilder::new();
        db.create(&build_dir)
            .expect(&format!(
                "couldn't create build directory: {}",
                build_dir.display()));
    }

    let mut reg = Handlebars::new();

    reg.register_template_string("Cargo.toml", include_str!("Cargo.toml.hbs")).unwrap();
    reg.register_template_string("lib.rs", include_str!("lib.rs.hbs")).unwrap();

    emit_cargo_toml(tcfg,&reg, &build_dir);
    emit_lib_rs(tcfg, &reg, &build_dir, modules);
}

/// Emit `lib.rs` for libraries and `main.rs` for binaries.
fn emit_lib_rs(tcfg: &TranspilerConfig, reg: &Handlebars, build_dir: &Path,
               modules: Vec<PathBuf>) {
    let plugin_args = tcfg.cross_check_configs
        .iter()
        .map(|ccc| format!("config_file = \"{}\"", ccc))
        .collect::<Vec<String>>()
        .join(", ");

    println!("{:?}", modules); // TODO: remove debug output
    let relpaths = modules
        .iter()
        // TODO: make path relative to `build_dir`
        .map(|m| (m.file_name().unwrap().to_str().unwrap()))
        .collect::<Vec<&str>>();

    let modnames = modules
        .iter()
        .map(|m: &PathBuf| {
            // remove .rs from filename
            let fname = m.file_name().unwrap().to_str().unwrap();
            let len = fname.len();
            &fname[0..len-3]
        })
        .collect::<Vec<&str>>();

    let modules = relpaths
        .iter()
        .zip(modnames.iter())
        .collect::<Vec<_>>();

    let json = json!({
        "reorganize_definitions": tcfg.reorganize_definitions,
        "cross_checks": tcfg.cross_checks,
        "use_fakechecks": tcfg.use_fakechecks,
        "main_module": tcfg.main,
        "plugin_args": plugin_args,
        "modules": modules
    });

    let file_name = match tcfg.main { Some(_) => "main.rs", None => "lib.rs" };
    let output_path = build_dir.join(file_name);
    let output = reg.render("lib.rs", &json).unwrap();

    maybe_write_to_file(&output_path, output);
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
    maybe_write_to_file(&output_path, output);
}

fn maybe_write_to_file(output_path: &Path, output: String) {
    if output_path.exists() {
        eprintln!("Skipping {}; file exists.", output_path.display());
        return;
    }

    let mut file = match File::create(&output_path) {
        Ok(file) => file,
        Err(e) => panic!("Unable to open file for writing: {}", e),
    };
    match file.write_all(output.as_bytes()) {
        Ok(()) => (),
        Err(e) => panic!("Unable to write translation to file: {}", e),
    };
}