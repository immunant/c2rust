use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::io::Write;
use std::ops::Bound::{Excluded, Unbounded};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use c2rust_rust_tools::rustfmt;
use c2rust_rust_tools::RustEdition::Edition2024;
use handlebars::Handlebars;
use itertools::Itertools;
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
/// Returns the path to `lib.rs` or `main.rs` (or `None` if the output file
/// existed already).
pub fn emit_build_files<'lcmd>(
    tcfg: &TranspilerConfig,
    build_dir: &Path,
    crate_cfg: Option<CrateConfig<'lcmd>>,
    workspace_members: Option<Vec<String>>,
) -> Option<PathBuf> {
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
    crate_cfg.and_then(|ccfg| {
        emit_build_rs(tcfg, &reg, build_dir, ccfg.link_cmd);
        emit_lib_rs(
            tcfg,
            &reg,
            build_dir,
            ccfg.modules,
            ccfg.pragmas,
            &ccfg.crates,
        )
    })
}

#[derive(Clone, Debug, Serialize)]
struct Module {
    path: Option<String>,
    name: String,
    open: bool,
    close: bool,
}

#[derive(Debug, Default)]
struct ModuleTree {
    children: BTreeMap<String, ModuleTree>,
    path_modules: Vec<Module>,
}

impl ModuleTree {
    /// Convert the tree representation into a linear vector
    /// and push it into `res`
    fn linearize(&self, res: &mut Vec<Module>) {
        res.extend(self.path_modules.iter().cloned());
        for (name, child) in self.children.iter() {
            child.linearize_internal(name, res);
        }
    }

    fn linearize_internal(&self, name: &str, res: &mut Vec<Module>) {
        if self.path_modules.is_empty() && self.children.is_empty() {
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

    fn insert_nested(&mut self, path: Vec<String>) {
        let mut cur = self;
        for name in path {
            cur = cur.children.entry(name).or_default();
        }
    }

    fn insert_path_module(
        &mut self,
        parent_path: &[String],
        file_name: String,
        module_name: String,
    ) {
        let mut cur = self;
        for name in parent_path {
            cur = cur.children.entry(name.clone()).or_default();
        }
        let name = cur.unique_child_name(&module_name);
        cur.path_modules.push(Module {
            path: Some(file_name),
            name,
            open: false,
            close: false,
        });
    }

    fn unique_child_name(&self, module_name: &str) -> String {
        if !self.child_name_exists(module_name) {
            return module_name.to_owned();
        }

        for i in 1.. {
            let candidate = format!("{module_name}_{i}");
            if !self.child_name_exists(&candidate) {
                return candidate;
            }
        }

        unreachable!("We tried all the numbers and couldn't find one that didn't collide")
    }

    fn child_name_exists(&self, module_name: &str) -> bool {
        self.children.contains_key(module_name)
            || self
                .path_modules
                .iter()
                .any(|module| module.name == module_name)
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
        let is_binary = tcfg.is_thin_or_full_binary(m);
        let is_binary_subset = module_subset == ModuleSubset::Binaries;
        // Don't add binary modules to lib.rs, these are emitted to
        // standalone, separate binary modules.
        // Retain "thin" binaries in both lists because
        // we include the code in the library crate,
        // but also wrap calls to their `main` functions
        // separately in the `c2rust-bin-{name}.rs` binary crates.
        if tcfg.thin_binaries {
            return is_binary || !is_binary_subset;
        }
        is_binary == is_binary_subset
    });

    let mut res = vec![];
    let mut module_tree = ModuleTree::default();
    // A C file can have the same stem as a sibling directory, such as
    // `hash.c` and `hash/sha1.c`. The current nested module emitter cannot
    // represent both at `mod hash`, so emit the file module with an explicit
    // path and keep the directory in the nested tree.
    let internal_modules = modules
        .iter()
        .filter_map(|m| {
            if tcfg.is_binary(m) {
                return None;
            }
            let relpath = m.strip_prefix(build_dir).ok()?;
            let path = module_path(relpath);
            Some((m, path))
        })
        .collect::<Vec<_>>();
    // A module collides when another module's path strictly extends it, i.e. a
    // file whose stem matches a sibling directory (`hash.c` next to `hash/`).
    // Every strict extension of a path sorts immediately after it, so checking
    // each path's immediate successor in the sorted set settles it without
    // comparing every pair.
    let module_paths = internal_modules
        .iter()
        .map(|(_, path)| path.clone())
        .collect::<BTreeSet<_>>();
    let collision_modules = internal_modules
        .iter()
        .filter(|(_, path)| {
            module_paths
                .range::<Vec<String>, _>((Excluded(path), Unbounded))
                .next()
                .is_some_and(|next| next.len() > path.len() && next.starts_with(path))
        })
        .map(|(module, _)| *module)
        .collect::<BTreeSet<_>>();

    let mut collision_relpaths = vec![];
    for m in &modules {
        match m.strip_prefix(build_dir) {
            Ok(relpath) if !tcfg.is_binary(m) && !collision_modules.contains(m) => {
                // The module is inside the build directory, use nested modules
                module_tree.insert_nested(module_path(relpath));
            }
            Ok(relpath) if !tcfg.is_binary(m) => {
                collision_relpaths.push(relpath.to_path_buf());
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
    for relpath in collision_relpaths {
        let mut path = module_path(&relpath);
        // Insert the shadowed file as an explicit `#[path]` module in its
        // parent. `insert_path_module` renames it if the stem collides with the
        // sibling directory (or another module), so pass the plain stem.
        let module_name = path.pop().unwrap();
        let file_name = relpath.file_name().unwrap().to_str().unwrap().to_string();
        module_tree.insert_path_module(&path, file_name, module_name);
    }
    module_tree.linearize(&mut res);
    res
}

fn module_path(path: &Path) -> Vec<String> {
    path.iter()
        .map(|component| {
            let path = Path::new(component);
            get_module_name(path, true, false, false).unwrap()
        })
        .collect()
}

fn emit_thin_binaries(
    tcfg: &TranspilerConfig,
    build_dir: &Path,
    crate_name: &str,
    modules: &mut Vec<Module>,
) {
    let mut crate_path = vec![crate_name.to_owned()];
    modules.retain_mut(|m| {
        if m.open {
            crate_path.push(m.name.to_owned());
            return false;
        }
        if m.close {
            let _ = crate_path.pop();
            return false;
        }

        // Emit a `c2rust-bin-{module_name}.rs` that just wraps the `main`
        // function from the actual module inside the crate.
        let Module { name, .. } = m;
        let file_name = format!("c2rust-bin-{name}.rs");
        let output_path = build_dir.join(&file_name);
        let main_path = crate_path.iter().join("::");
        let output = format!(
            r"
fn main() {{
    ::{main_path}::{name}::main()
}}
"
        );
        maybe_write_to_file(&output_path, &output, tcfg.overwrite_existing);

        // Update the path written to `Cargo.toml`
        m.path = Some(file_name.into());
        true
    });
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
    let crates = convert_dependencies_list(crates.clone(), tcfg.c2rust_dir.as_deref());
    let file_name = get_lib_rs_file_name(tcfg);
    let rs_xcheck_backend = tcfg.cross_check_backend.replace('-', "_");
    let json = json!({
        "lib_rs_file": file_name,
        "cross_checks": tcfg.cross_checks,
        "cross_check_backend": rs_xcheck_backend,
        "plugin_args": plugin_args,
        "modules": modules,
        "pragmas": pragmas,
        "crates": crates,
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
        let mut binaries = convert_module_list(
            tcfg,
            build_dir,
            ccfg.modules.to_owned(),
            ModuleSubset::Binaries,
        );
        if tcfg.thin_binaries {
            emit_thin_binaries(tcfg, build_dir, &ccfg.crate_name, &mut binaries);
        }

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
