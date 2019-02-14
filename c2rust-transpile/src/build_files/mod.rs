extern crate handlebars;
extern crate pathdiff;

use std::fs::File;
use std::fs::DirBuilder;
use std::path::{Path, PathBuf};
use std::io::Write;
use std::str::FromStr;

use self::handlebars::Handlebars;
use self::pathdiff::diff_paths;
use serde_json::json;

use super::TranspilerConfig;

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
            _ => Err(())
        }
    }
}

/// Create the build directory
pub fn get_build_dir(tcfg: &TranspilerConfig, cc_db: &Path) -> PathBuf {
    let cc_db_dir = cc_db
        .parent() // get directory of `compile_commands.json`
        .unwrap();

    if !tcfg.emit_build_files {
        return cc_db_dir.into();
    }

    if let BuildDirectoryContents::Nothing = tcfg.build_directory_contents {
        // We do not put anything in the build directory;
        // everything, including `Cargo.toml` and `lib.rs`, goes in the
        // same place as the compilation database
        return cc_db_dir.into();
    }

    let db = DirBuilder::new();
    let build_dir = cc_db_dir.join(&tcfg.build_directory_name);
    if !build_dir.exists() {
        db.create(&build_dir).expect(&format!(
            "couldn't create build directory: {}",
            build_dir.display()
        ));
    }

    if let BuildDirectoryContents::Full = tcfg.build_directory_contents {
        let build_src_dir = build_dir.join("src");
        if !build_src_dir.exists() {
            db.create(&build_src_dir).expect(&format!(
                "couldn't create build source directory: {}",
                build_src_dir.display()
            ));
        }
    }

    build_dir
}

/// Emit `Cargo.toml` and `lib.rs` for a library or `main.rs` for a binary.
/// Returns the path to `lib.rs` or `main.rs` (or `None` if the output file
/// existed already).
pub fn emit_build_files(tcfg: &TranspilerConfig, build_dir: &Path,
                        modules: Vec<PathBuf>) -> Option<PathBuf> {

    let mut reg = Handlebars::new();

    reg.register_template_string("Cargo.toml", include_str!("Cargo.toml.hbs")).unwrap();
    reg.register_template_string("lib.rs", include_str!("lib.rs.hbs")).unwrap();

    emit_cargo_toml(tcfg,&reg, &build_dir);
    emit_lib_rs(tcfg, &reg, &build_dir, modules)
}

#[derive(Serialize)]
struct Module {
    path: String,
    name: String,
}

fn get_root_rs_file_name(tcfg: &TranspilerConfig) -> &str {
    match (&tcfg.main, &tcfg.build_directory_contents) {
        (Some(_), BuildDirectoryContents::Nothing) => "c2rust-main.rs",
        (None, BuildDirectoryContents::Nothing) => "c2rust-lib.rs",
        (Some(_), _) => "main.rs",
        (None, _) => "lib.rs",
    }
}

/// Emit `lib.rs` for a library or `main.rs` for a binary. Returns the path
/// to `lib.rs` or `main.rs` (or `None` if the output file existed already).
fn emit_lib_rs(tcfg: &TranspilerConfig, reg: &Handlebars, build_dir: &Path,
               modules: Vec<PathBuf>) -> Option<PathBuf> {
    let plugin_args = tcfg.cross_check_configs
        .iter()
        .map(|ccc| format!("config_file = \"{}\"", ccc))
        .collect::<Vec<String>>()
        .join(", ");

    let modules = modules
        .iter()
        .map(|m| {
            let relpath = diff_paths(m, build_dir).unwrap();
            let name = m.file_stem().unwrap().to_str().unwrap();
            Module {
                path: relpath.to_str().unwrap().to_string(),
                name: name.to_string(),
            }
        })
        .collect::<Vec<_>>();

    let file_name = get_root_rs_file_name(tcfg);
    let rs_xcheck_backend = tcfg.cross_check_backend.replace("-", "_");
    let json = json!({
        "root_rs_file": file_name,
        "reorganize_definitions": tcfg.reorganize_definitions,
        "cross_checks": tcfg.cross_checks,
        "cross_check_backend": rs_xcheck_backend,
        "main_module": tcfg.main,
        "plugin_args": plugin_args,
        "modules": modules
    });

    let output_path = build_dir.join(file_name);
    let output = reg.render("lib.rs", &json).unwrap();

    maybe_write_to_file(&output_path, output, tcfg.overwrite_existing)
}

fn emit_cargo_toml(tcfg: &TranspilerConfig, reg: &Handlebars, build_dir: &Path) {
    // rust_checks_path is gone because we don't want to refer to the source
    // path but instead want the cross-check libs to be installed via cargo.
    let json = json!({
        "crate_name": tcfg.build_directory_name,
        "root_rs_file": get_root_rs_file_name(tcfg),
        "main_module": tcfg.main,
        "cross_checks": tcfg.cross_checks,
        "cross_check_backend": tcfg.cross_check_backend,
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
