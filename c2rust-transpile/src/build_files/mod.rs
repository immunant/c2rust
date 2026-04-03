use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use c2rust_rust_tools::rustfmt;
use c2rust_rust_tools::RustEdition::Edition2024;
use handlebars::Handlebars;
use pathdiff::diff_paths;
use serde_derive::Serialize;
use serde_json::json;

use super::compile_cmds::LinkCmd;
use super::TranspilerConfig;
use crate::get_module_name;
use crate::CrateSet;
use crate::ExternCrateDetails;
use crate::PragmaSet;

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
                fs::create_dir(&output_dir).unwrap_or_else(|_| {
                    panic!("couldn't create build directory: {}", output_dir.display())
                });
            }
            output_dir
        }
        None => cc_db_dir.into(),
    }
}

pub struct CrateConfig<'lcmd> {
    pub crate_name: String,
    pub modules: Vec<PathBuf>,
    pub pragmas: PragmaSet,
    pub crates: CrateSet,
    pub link_cmd: &'lcmd LinkCmd,
}

/// Emit `Cargo.toml` and `lib.rs` for a library or `main.rs` for a binary.
/// Returns a single-element vector with the path to `lib.rs` or `main.rs`
/// (or `[]` if the output file existed already). This may return multiple
/// elements if there are multiple binaries in the current session, e.g.,
/// if the `--no-split-library` command line flag is present.
pub fn emit_build_files<'lcmd>(
    tcfg: &TranspilerConfig,
    build_dir: &Path,
    crate_cfg: Option<CrateConfig<'lcmd>>,
    workspace_members: Option<Vec<String>>,
) -> Vec<PathBuf> {
    let mut reg = Handlebars::new();

    reg.register_template_string("Cargo.toml", include_str!("Cargo.toml.hbs"))
        .unwrap();
    reg.register_template_string("lib.rs", include_str!("lib.rs.hbs"))
        .unwrap();
    reg.register_template_string("build.rs", include_str!("build.rs.hbs"))
        .unwrap();

    if !build_dir.exists() {
        fs::create_dir_all(build_dir)
            .unwrap_or_else(|_| panic!("couldn't create build directory: {}", build_dir.display()));
    }

    emit_cargo_toml(tcfg, &reg, build_dir, &crate_cfg, workspace_members);
    if tcfg.translate_valist {
        emit_rust_toolchain(tcfg, build_dir);
    }
    // `let ... else` is not supported on both the old nightly and current stable
    let ccfg = if let Some(ccfg) = crate_cfg {
        ccfg
    } else {
        return vec![];
    };

    emit_build_rs(tcfg, &reg, build_dir, ccfg.link_cmd);
    if tcfg.no_split_library {
        tcfg.binaries
            .iter()
            .flat_map(|binary| {
                emit_lib_rs(
                    tcfg,
                    &reg,
                    build_dir,
                    ccfg.modules.clone(),
                    &ccfg.pragmas,
                    &ccfg.crates,
                    Some(binary),
                )
                .into_iter()
            })
            .collect()
    } else {
        emit_lib_rs(
            tcfg,
            &reg,
            build_dir,
            ccfg.modules.clone(),
            &ccfg.pragmas,
            &ccfg.crates,
            None,
        )
        .into_iter()
        .collect()
    }
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
            res.push(Module {
                name: name.to_string(),
                path: None,
                open: false,
                close: false,
            });
        } else {
            res.push(Module {
                name: name.to_string(),
                path: None,
                open: true,
                close: false,
            });
            self.linearize(res);
            res.push(Module {
                name: name.to_string(),
                path: None,
                open: false,
                close: true,
            });
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
    current_binary: &Option<&str>,
) -> Vec<Module> {
    modules.retain(|m| {
        let binary_name = tcfg.maybe_binary_name_from_path(m);
        let is_binary = binary_name.is_some();
        let is_binary_subset = module_subset == ModuleSubset::Binaries;
        // Don't add binary modules to lib.rs, these are emitted to
        // standalone, separate binary modules.
        if tcfg.no_split_library && is_binary && &binary_name.as_deref() == current_binary {
            // Preserve the current binary in no_split_library mode
            // because we include it in the top-level `c2rust-bin-{name}.rs`.
            return true;
        }
        is_binary == is_binary_subset
    });

    // Whether to include `current_binary` into our "library" build which
    // is actually `c2rust-bin-{name}.rs`.
    let include_binary = tcfg.no_split_library && module_subset == ModuleSubset::Libraries;

    let mut res = vec![];
    let mut module_tree = ModuleTree(BTreeMap::new());
    for m in &modules {
        match m.strip_prefix(build_dir) {
            Ok(relpath) if !tcfg.is_binary(m) || include_binary => {
                // The module is inside the build directory, use nested modules
                let mut cur = &mut module_tree;
                for sm in relpath.iter() {
                    let path = Path::new(sm);
                    let name = get_module_name(path, true, false, false).unwrap();
                    cur = cur.0.entry(name).or_default();
                }
            }
            _ if tcfg.no_split_library && tcfg.is_binary(m) => {
                // We use `c2rust-bin-{name}.rs` when emitting `Cargo.toml`
                // but the actual name for `emit_lib_rs`.
                assert!(module_subset == ModuleSubset::Binaries);

                // TODO: does this need check_reserved?
                let name = TranspilerConfig::binary_name_from_path(m);
                let path = format!("c2rust-bin-{}.rs", name).into();
                res.push(Module {
                    path,
                    name,
                    open: false,
                    close: false,
                });
            }
            _ => {
                let relpath = diff_paths(m, build_dir).unwrap();
                let path = Some(relpath.to_str().unwrap().to_string());
                let name = get_module_name(m, true, false, false).unwrap();
                res.push(Module {
                    path,
                    name,
                    open: false,
                    close: false,
                });
            }
        }
    }
    module_tree.linearize(&mut res);
    res
}

fn convert_dependencies_list(
    crates: CrateSet,
    c2rust_dir: Option<&Path>,
) -> Vec<ExternCrateDetails> {
    crates
        .into_iter()
        .map(|dep| dep.with_details(c2rust_dir))
        .collect()
}

fn get_lib_rs_file_name(tcfg: &TranspilerConfig) -> &str {
    if tcfg.output_dir.is_some() {
        "lib.rs"
    } else {
        "c2rust-lib.rs"
    }
}

/// Emit `build.rs` to make it easier to link in native libraries
fn emit_build_rs(
    tcfg: &TranspilerConfig,
    reg: &Handlebars,
    build_dir: &Path,
    link_cmd: &LinkCmd,
) -> Option<PathBuf> {
    let json = json!({
        "libraries": link_cmd.libs,
    });
    let output = reg.render("build.rs", &json).unwrap();
    let output_path = build_dir.join("build.rs");
    let path = maybe_write_to_file(&output_path, &output, tcfg.overwrite_existing)?;

    if !tcfg.disable_rustfmt {
        rustfmt(&output_path).edition(tcfg.edition).run();
    }

    Some(path)
}

/// Emit lib.rs (main.rs) for a library (binary). Returns `Some(path)`
/// to the generated file or `None` if the output file exists.
fn emit_lib_rs(
    tcfg: &TranspilerConfig,
    reg: &Handlebars,
    build_dir: &Path,
    modules: Vec<PathBuf>,
    pragmas: &PragmaSet,
    crates: &CrateSet,
    current_binary: Option<&str>,
) -> Option<PathBuf> {
    let plugin_args = tcfg
        .cross_check_configs
        .iter()
        .map(|ccc| format!("config_file = \"{}\"", ccc))
        .collect::<Vec<String>>()
        .join(", ");

    let mut current_binary_path = String::new();
    if current_binary.is_some() {
        for elem in modules
            .iter()
            .find(|m| tcfg.maybe_binary_name_from_path(m).as_deref() == current_binary)
            .unwrap()
            .strip_prefix(build_dir)
            .unwrap()
            .iter()
        {
            if !current_binary_path.is_empty() {
                current_binary_path.push_str("::");
            }

            let name = get_module_name(Path::new(elem), true, false, false).unwrap();
            current_binary_path.push_str(&name);
        }
    };

    let modules = convert_module_list(
        tcfg,
        build_dir,
        modules,
        ModuleSubset::Libraries,
        &current_binary,
    );
    let crates = convert_dependencies_list(crates.clone(), tcfg.c2rust_dir.as_deref());
    let file_name = if let Some(current_binary) = current_binary {
        format!("c2rust-bin-{}.rs", current_binary)
    } else {
        get_lib_rs_file_name(tcfg).to_string()
    };
    let rs_xcheck_backend = tcfg.cross_check_backend.replace('-', "_");
    let json = json!({
        "lib_rs_file": file_name,
        "cross_checks": tcfg.cross_checks,
        "cross_check_backend": rs_xcheck_backend,
        "plugin_args": plugin_args,
        "modules": modules,
        "pragmas": pragmas,
        "crates": crates,
        "current_binary_path": current_binary_path,
    });

    let output_path = build_dir.join(file_name);
    let output = reg.render("lib.rs", &json).unwrap();
    let path = maybe_write_to_file(&output_path, &output, tcfg.overwrite_existing)?;

    if !tcfg.disable_rustfmt {
        rustfmt(&output_path).edition(tcfg.edition).run();
    }

    Some(path)
}

/// If we translate variadic functions, the output will only compile
/// on a nightly toolchain until the `c_variadics` feature is stable.
fn emit_rust_toolchain(tcfg: &TranspilerConfig, build_dir: &Path) {
    let output_path = build_dir.join("rust-toolchain.toml");
    let toolchain = tcfg.edition.toolchain().strip_prefix("+").unwrap();
    let output = format!(
        r#"
[toolchain]
channel = "{toolchain}"
components = ["rustfmt"]
"#
    );
    let output = output.trim_start();
    maybe_write_to_file(&output_path, output, tcfg.overwrite_existing);
}

fn emit_cargo_toml<'lcmd>(
    tcfg: &TranspilerConfig,
    reg: &Handlebars,
    build_dir: &Path,
    crate_cfg: &Option<CrateConfig<'lcmd>>,
    workspace_members: Option<Vec<String>>,
) {
    // rust_checks_path is gone because we don't want to refer to the source
    // path but instead want the cross-check libs to be installed via cargo.
    let mut json = json!({
        "is_workspace": workspace_members.is_some(),
        "is_crate": crate_cfg.is_some(),
        "workspace_members": workspace_members.unwrap_or_default(),
    });
    if let Some(ccfg) = crate_cfg {
        let binaries = convert_module_list(
            tcfg,
            build_dir,
            ccfg.modules.to_owned(),
            ModuleSubset::Binaries,
            &None,
        );
        let dependencies =
            convert_dependencies_list(ccfg.crates.clone(), tcfg.c2rust_dir.as_deref());
        let crate_json = json!({
            "crate_name": ccfg.crate_name,
            "crate_rust_name": ccfg.crate_name.replace('-', "_"),
            "edition": tcfg.edition.as_str(),
            // This is already the default in Rust 1.77,
            // and edition 2024 was released in Rust 1.85.
            "strip_debuginfo_release": tcfg.edition < Edition2024,
            "crate_types": ccfg.link_cmd.r#type.as_cargo_types(),
            "is_library": ccfg.link_cmd.r#type.is_library(),
            "lib_rs_file": get_lib_rs_file_name(tcfg),
            "binaries": binaries,
            "cross_checks": tcfg.cross_checks,
            "cross_check_backend": tcfg.cross_check_backend,
            "dependencies": dependencies,
            "no_split_library": tcfg.no_split_library,
        });
        json.as_object_mut().unwrap().extend(
            crate_json
                .as_object()
                .cloned() // FIXME: we need to clone it because there's no `into_object`
                .unwrap(),
        );
    }

    let file_name = "Cargo.toml";
    let output_path = build_dir.join(file_name);
    let output = reg.render(file_name, &json).unwrap();
    maybe_write_to_file(&output_path, &output, tcfg.overwrite_existing);
}

fn maybe_write_to_file(output_path: &Path, output: &str, overwrite: bool) -> Option<PathBuf> {
    if output_path.exists() && !overwrite {
        eprintln!("Skipping existing file {}", output_path.display());
        return None;
    }

    let mut file = match File::create(output_path) {
        Ok(file) => file,
        Err(e) => panic!("Unable to open file for writing: {}", e),
    };
    match file.write_all(output.as_bytes()) {
        Ok(()) => (),
        Err(e) => panic!("Unable to write translation to file: {}", e),
    };

    Some(PathBuf::from(output_path))
}
