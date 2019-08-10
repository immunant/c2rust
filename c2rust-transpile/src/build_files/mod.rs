extern crate handlebars;
extern crate pathdiff;

use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use self::handlebars::Handlebars;
use self::pathdiff::diff_paths;
use serde_json::json;

use super::TranspilerConfig;
use crate::CrateSet;
use crate::PragmaSet;
use crate::get_module_name;

#[derive(Debug, Copy, Clone)]
pub enum BuildDirectoryContents {
    Nothing,
    Minimal,
    Full,
}

impl FromStr for BuildDirectoryContents {
    type Err = ();

    fn from_str(s: &str) -> Result<BuildDirectoryContents, ()> {
        match s {
            "nothing" => Ok(BuildDirectoryContents::Nothing),
            "minimal" => Ok(BuildDirectoryContents::Minimal),
            "full" => Ok(BuildDirectoryContents::Full),
            _ => Err(()),
        }
    }
}

/// Create the build directory
pub fn get_build_dir(tcfg: &TranspilerConfig, cc_db: &Path) -> PathBuf {
    let cc_db_dir = cc_db
        .parent() // get directory of `compile_commands.json`
        .unwrap();

    match &tcfg.output_dir {
        Some(dir) => {
            let output_dir = dir.clone();
            if !output_dir.exists() {
                fs::create_dir(&output_dir).expect(&format!(
                    "couldn't create build directory: {}",
                    output_dir.display()
                ));
            }
            output_dir
        }
        None => cc_db_dir.into(),
    }
}

/// Emit `Cargo.toml` and `lib.rs` for a library or `main.rs` for a binary.
/// Returns the path to `lib.rs` or `main.rs` (or `None` if the output file
/// existed already).
pub fn emit_build_files(
    tcfg: &TranspilerConfig,
    build_dir: &Path,
    modules: Vec<PathBuf>,
    pragmas: PragmaSet,
    crates: CrateSet,
) -> Option<PathBuf> {
    let mut reg = Handlebars::new();

    reg.register_template_string("Cargo.toml", include_str!("Cargo.toml.hbs"))
        .unwrap();
    reg.register_template_string("lib.rs", include_str!("lib.rs.hbs"))
        .unwrap();
    reg.register_template_string("build.rs", include_str!("build.rs.hbs"))
        .unwrap();

    emit_cargo_toml(tcfg, &reg, &build_dir, &modules, &crates);
    if tcfg.translate_valist {
        emit_rust_toolchain(tcfg, &build_dir);
    }
    emit_build_rs(tcfg, &reg, &build_dir);
    emit_lib_rs(tcfg, &reg, &build_dir, modules, pragmas, &crates)
}

#[derive(Serialize)]
struct Module {
    path: String,
    name: String,
}

fn get_lib_rs_file_name(tcfg: &TranspilerConfig) -> &str {
    if tcfg.output_dir.is_some() {
        "lib.rs"
    } else {
        "c2rust-lib.rs"
    }
}

/// Emit `build.rs` to make it easier to link in native libraries
fn emit_build_rs(tcfg: &TranspilerConfig, reg: &Handlebars, build_dir: &Path) -> Option<PathBuf> {
    let json = json!({});
    let output = reg.render("build.rs", &json).unwrap();
    let output_path = build_dir.join("build.rs");
    maybe_write_to_file(&output_path, output, tcfg.overwrite_existing)
}

/// Emit lib.rs (main.rs) for a library (binary). Returns `Some(path)`
/// to the generated file or `None` if the output file exists.
fn emit_lib_rs(
    tcfg: &TranspilerConfig,
    reg: &Handlebars,
    build_dir: &Path,
    modules: Vec<PathBuf>,
    pragmas: PragmaSet,
    crates: &CrateSet,
) -> Option<PathBuf> {
    let plugin_args = tcfg
        .cross_check_configs
        .iter()
        .map(|ccc| format!("config_file = \"{}\"", ccc))
        .collect::<Vec<String>>()
        .join(", ");

    let modules = modules
        .iter()
        .filter_map(|m| {
            if tcfg.is_binary(&m) {
                // Don't add binary modules to lib.rs, these are emitted to
                // standalone, separate binary modules.
                None
            } else {
                let relpath = diff_paths(m, build_dir).unwrap();
                let path = relpath.to_str().unwrap().to_string();
                let fname = &m.file_stem().unwrap().to_str().map(String::from);
                let name = get_module_name(fname).unwrap();
                Some(Module { path, name })
            }
        })
        .collect::<Vec<_>>();

    let file_name = get_lib_rs_file_name(tcfg);
    let rs_xcheck_backend = tcfg.cross_check_backend.replace("-", "_");
    let json = json!({
        "lib_rs_file": file_name,
        "reorganize_definitions": tcfg.reorganize_definitions,
        "translate_valist": tcfg.translate_valist,
        "cross_checks": tcfg.cross_checks,
        "cross_check_backend": rs_xcheck_backend,
        "plugin_args": plugin_args,
        "modules": modules,
        "pragmas": pragmas,
        "crates": crates,
    });

    let output_path = build_dir.join(file_name);
    let output = reg.render("lib.rs", &json).unwrap();

    maybe_write_to_file(&output_path, output, tcfg.overwrite_existing)
}

/// If we translate variadic functions, the output will only compile
/// on a nightly toolchain until the `c_variadics` feature is stable.
fn emit_rust_toolchain(tcfg: &TranspilerConfig, build_dir: &Path) {
    let output_path = build_dir.join("rust-toolchain");
    let output = include_str!("../../rust-toolchain").to_string();
    maybe_write_to_file(&output_path, output, tcfg.overwrite_existing);
}

fn emit_cargo_toml(
    tcfg: &TranspilerConfig,
    reg: &Handlebars,
    build_dir: &Path,
    modules: &[PathBuf],
    crates: &CrateSet,
) {
    // rust_checks_path is gone because we don't want to refer to the source
    // path but instead want the cross-check libs to be installed via cargo.

    let binaries = modules
        .iter()
        .filter_map(|m| {
            if tcfg.is_binary(&m) {
                let relpath = diff_paths(m, build_dir).unwrap();
                let path = relpath.to_str().unwrap().to_string();
                let fname = &m.file_stem().unwrap().to_str().map(String::from);
                let name = get_module_name(fname).unwrap();
                Some(Module { path, name })
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let json = json!({
        "crate_name": tcfg.crate_name(),
        "crate_rust_name": tcfg.crate_name().replace('-', "_"),
        "lib_rs_file": get_lib_rs_file_name(tcfg),
        "binaries": binaries,
        "cross_checks": tcfg.cross_checks,
        "cross_check_backend": tcfg.cross_check_backend,
        "c2rust_bitfields": crates.contains("c2rust_bitfields"),
        "f128": crates.contains("f128"),
        "num_traits": crates.contains("num_traits"),
    });
    let file_name = "Cargo.toml";
    let output_path = build_dir.join(file_name);
    let output = reg.render(file_name, &json).unwrap();
    maybe_write_to_file(&output_path, output, tcfg.overwrite_existing);
}

fn maybe_write_to_file(output_path: &Path, output: String, overwrite: bool) -> Option<PathBuf> {
    if output_path.exists() && !overwrite {
        eprintln!("Skipping existing file {}", output_path.display());
        return None;
    }

    let mut file = match File::create(&output_path) {
        Ok(file) => file,
        Err(e) => panic!("Unable to open file for writing: {}", e),
    };
    match file.write_all(output.as_bytes()) {
        Ok(()) => (),
        Err(e) => panic!("Unable to write translation to file: {}", e),
    };

    Some(PathBuf::from(output_path))
}
