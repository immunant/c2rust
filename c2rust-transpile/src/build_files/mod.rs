extern crate handlebars;
extern crate pathdiff;

use std::collections::BTreeMap;
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
use crate::get_path_module_name;

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
    path: Option<String>,
    name: String,
    open: bool,
    close: bool,
}

#[derive(Debug, Default)]
struct ModuleTree(BTreeMap<String, ModuleTree>);

impl ModuleTree {
    /// Convert the tree representation into a linear vector
    /// and push it into `res`
    fn linearize(&self, res: &mut Vec<Module>) {
        for (name, child) in self.0.iter() {
            child.linearize_internal(name, res);
        }
    }

    fn linearize_internal(&self, name: &str, res: &mut Vec<Module>) {
        if self.0.is_empty() {
            res.push(Module { name: name.to_string(), path: None, open: false, close: false });
        } else {
            res.push(Module { name: name.to_string(), path: None, open: true, close: false });
            self.linearize(res);
            res.push(Module { name: name.to_string(), path: None, open: false, close: true });
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ModuleSubset {
    Binaries,
    Libraries,
    //Both,
}

fn convert_module_list(
    tcfg: &TranspilerConfig,
    build_dir: &Path,
    mut modules: Vec<PathBuf>,
    module_subset: ModuleSubset,
) -> Vec<Module> {
    modules.retain(|m| {
        let is_binary = tcfg.is_binary(&m);
        if is_binary && module_subset == ModuleSubset::Libraries {
            // Don't add binary modules to lib.rs, these are emitted to
            // standalone, separate binary modules.
            false
        } else if !is_binary && module_subset == ModuleSubset::Binaries {
            false
        } else {
            true
        }
    });

    let mut res = vec![];
    let mut module_tree = ModuleTree(BTreeMap::new());
    for m in &modules {
        if m.starts_with(build_dir) {
            // The module is inside the build directory, use nested modules
            let relpath = m.strip_prefix(build_dir)
                .expect("Couldn't strip path prefix");
            let mut cur = &mut module_tree;
            for sm in relpath.iter() {
                let path = Path::new(sm);
                let name = get_path_module_name(&path, true, false).unwrap();
                cur = cur.0.entry(name).or_default();
            }
        } else {
            let relpath = diff_paths(m, build_dir).unwrap();
            let path = Some(relpath.to_str().unwrap().to_string());
            let name = get_path_module_name(m, true, false).unwrap();
            res.push(Module { path, name, open: false, close: false });
        }
    }
    module_tree.linearize(&mut res);
    res
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

    let modules = convert_module_list(tcfg, build_dir, modules, ModuleSubset::Libraries);
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

    let binaries = convert_module_list(tcfg, build_dir, modules.to_owned(), ModuleSubset::Binaries);
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
