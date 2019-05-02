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
use crate::convert_type::RESERVED_NAMES;

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

    emit_cargo_toml(tcfg, &reg, &build_dir, &crates);
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

fn get_root_rs_file_name(tcfg: &TranspilerConfig) -> &str {
    match (&tcfg.main, &tcfg.output_dir) {
        (Some(_), None) => "c2rust-main.rs",
        (None, None) => "c2rust-lib.rs",
        (Some(_), Some(_)) => "main.rs",
        (None, Some(_)) => "lib.rs",
    }
}

/// Make sure that module name:
/// - does not contain illegal characters,
/// - does not clash with reserved keywords.
fn get_module_name(main: &Option<String>) -> Option<String> {
    if let Some(ref name) = main {
        // module names cannot contain periods or dashes
        let mut module = name.chars().map(|c|
            match c {
                '.' | '-' => '_',
                _ => c
            }
        ).collect();

        // make sure the module name does not clash with keywords
        if RESERVED_NAMES.contains(&name.as_str()) {
            module = format!("r#{}", module);
        }
        return Some(module);
    }
    None
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
        .map(|m| {
            let relpath = diff_paths(m, build_dir).unwrap();
            let path = relpath.to_str().unwrap().to_string();
            let fname = &m.file_stem().unwrap().to_str().map(String::from);
            let name = get_module_name(fname).unwrap();
            Module { path, name }
        })
        .collect::<Vec<_>>();

    let file_name = get_root_rs_file_name(tcfg);
    let rs_xcheck_backend = tcfg.cross_check_backend.replace("-", "_");
    let json = json!({
        "root_rs_file": file_name,
        "reorganize_definitions": tcfg.reorganize_definitions,
        "translate_valist": tcfg.translate_valist,
        "cross_checks": tcfg.cross_checks,
        "cross_check_backend": rs_xcheck_backend,
        "main_module": get_module_name(&tcfg.main),
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

fn emit_cargo_toml(tcfg: &TranspilerConfig, reg: &Handlebars, build_dir: &Path, crates: &CrateSet) {
    // rust_checks_path is gone because we don't want to refer to the source
    // path but instead want the cross-check libs to be installed via cargo.
    let json = json!({
        "crate_name": tcfg.output_dir.as_ref().and_then(
            |x| x.file_name().map(|x| x.to_string_lossy())
        ).unwrap_or("c2rust".into()),
        "root_rs_file": get_root_rs_file_name(tcfg),
        "main_module": tcfg.main.is_some(),
        "cross_checks": tcfg.cross_checks,
        "cross_check_backend": tcfg.cross_check_backend,
        "c2rust_bitfields": crates.contains("c2rust_bitfields"),
        "f128": crates.contains("f128"),
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
