extern crate handlebars;

use std::env;

use self::handlebars::Handlebars;
use super::serde_json::{json};

use super::TranspilerConfig;

pub fn emit_build_files(tcfg: &TranspilerConfig) {

    let mut reg = Handlebars::new();

    reg.register_template_string("Cargo.toml", include_str!("Cargo.toml.hbs")).unwrap();
    reg.register_template_string("lib.rs", include_str!("lib.rs.hbs")).unwrap();

    emit_cargo_toml(tcfg,&reg);
    emit_lib_rs(tcfg, &reg);
}

/// Emit `lib.rs` for libraries and `main.rs` for binaries.
fn emit_lib_rs(tcfg: &TranspilerConfig, reg: &Handlebars) {
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
    println!("{}", reg.render("lib.rs", &json).unwrap());
}

fn emit_cargo_toml(tcfg: &TranspilerConfig, reg: &Handlebars) {
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
    println!("{}", reg.render("Cargo.toml", &json).unwrap());
}