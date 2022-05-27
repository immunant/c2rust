use core::panic;
use std::{
    collections::HashSet,
    error::Error,
    ffi::OsStr,
    fs::{self, File},
    io::{self, BufRead, BufReader, ErrorKind},
    path::{Path, PathBuf},
    process::{self, Command},
};

use c2rust_transpile::{transpile, TranspilerConfig};
use ignore::{types::TypesBuilder, WalkBuilder};
use itertools::Itertools;
// use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

type AnyError = Box<dyn Error + Send + Sync + 'static>;

fn run_command(mut cmd: Command) -> Result<(), AnyError> {
    let status = cmd.status()?;
    if !status.success() {
        return Err(format!("non-zero exit status: {} from {:?}", status, cmd).into());
    }
    Ok(())
}

#[derive(Default)]
struct Directives {
    directives: HashSet<String>,
    top_level_attributes: Vec<String>,
}

impl Directives {
    fn new() -> Self {
        Self::default()
    }

    fn try_add_directive(&mut self, line: &str) -> Option<()> {
        let directive = line.strip_prefix("//!")?;
        self.directives
            .extend(directive.split(",").map(|s| s.trim()).map(|s| s.to_owned()));
        Some(())
    }

    fn try_add_top_level_attribute(&mut self, line: &str) -> Option<()> {
        let line = line.strip_prefix("#![")?;
        let line = line.strip_suffix("]")?;
        if let Some(line) = line.strip_suffix(")") {
            // split them up so we can deduplicate them easily
            let parts = line.split("(").collect_vec();
            let (attr_name, attr_values) = match parts.as_slice() {
                &[attr_name, attr_values] => (attr_name, attr_values),
                _ => panic!("expected {}) to have only one pair of parentheses", line),
            };
            let attr_values = attr_values.split(",").map(|s| s.trim());
            self.top_level_attributes
                .extend(attr_values.map(|attr_value| format!("#![{attr_name}({attr_value})]")))
        } else {
            self.top_level_attributes.push(line.to_owned());
        }
        Some(())
    }

    fn read(path: &Path) -> io::Result<Self> {
        let mut this = Self::new();
        let file = File::open(path)?;
        for line in BufReader::new(file).lines() {
            let line = line?;
            let line = line.as_str();
            let line = line.trim();
            this.try_add_directive(line);
            this.try_add_top_level_attribute(line);
            if line.starts_with("#![") && line.ends_with("]") {
                this.try_add_top_level_attribute(line);
            } else if let Some(directive) = line.strip_prefix("//!") {
                this.try_add_directive(directive);
            }
        }
        Ok(this)
    }

    fn skip(&self) -> bool {
        ["skip_translation", "xfail"]
            .iter()
            .any(|directive| self.directives.contains(*directive))
    }

    fn directive_extern_crates(&self) -> impl Iterator<Item = &str> {
        self.directives
            .iter()
            .filter_map(|directive| directive.strip_prefix("extern_crate_"))
    }

    fn directive_features(&self) -> impl Iterator<Item = &str> {
        self.directives
            .iter()
            .filter_map(|directive| directive.strip_prefix("feature_"))
    }

    fn top_level_attributes(&self) -> impl Iterator<Item = String> + '_ {
        let originals = self.top_level_attributes.iter().map(|s| s.to_owned());
        let features = self
            .directive_features()
            .map(|s| format!("#![feature({s})]"));
        let extern_crates = self
            .directive_extern_crates()
            .map(|s| format!("#[macro_use] extern crate {s};"));
        originals.chain(features).chain(extern_crates)
    }
}

struct TestDir {
    dir: PathBuf,
    src_dir: PathBuf,
    target: Option<String>,
}

impl TestDir {
    fn new(dir: &Path) -> Result<Self, AnyError> {
        let dir = fs::canonicalize(dir)?;
        let src_dir = dir.join("src");
        let target = match fs::read_to_string(dir.join("target-tuple")) {
            Ok(target) => Some(target.trim().to_owned()),
            Err(e) => match e.kind() {
                ErrorKind::NotFound => None,
                _ => return Err(e.into()),
            },
        };
        Ok(Self {
            dir,
            src_dir,
            target,
        })
    }

    fn target_args(&self) -> impl IntoIterator<Item = &str> {
        match self.target.as_deref() {
            Some(target) => ["-target", target],
            None => ["-march=native", ""],
        }
    }

    fn test_file(&self, c_file: PathBuf) -> Result<Option<json_compilation_db::Entry>, AnyError> {
        if Directives::read(&c_file)?.skip() {
            return Ok(None);
        }
        let o_file = c_file.with_extension("o");
        let mut cmd = Command::new("clang");
        cmd.args(&["-fPIC", "-D_FORTIFY_SOURCE=0"])
            .args(self.target_args())
            .arg("-c")
            .arg("-o")
            .arg(&o_file)
            .arg(&c_file);
        let cc_db_entry = json_compilation_db::Entry {
            file: c_file.to_path_buf(),
            arguments: cmd
                .get_args()
                .map(|s| s.to_string_lossy().into_owned())
                .collect(),
            directory: self.src_dir.to_path_buf(),
            output: Some(o_file.to_path_buf()),
        };
        run_command(cmd)?;
        Ok(Some(cc_db_entry))
    }

    fn test_dir(&self, keep_artifacts: bool) -> Result<(), AnyError> {
        let cc_db_entries = WalkBuilder::new(self.src_dir.as_path())
            .standard_filters(true)
            .types(TypesBuilder::new().add_defaults().select("c").build()?)
            .build()
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.file_type()
                    .map(|file_type| file_type.is_file())
                    .unwrap_or(false)
                    && Path::new(e.file_name()).extension() == Some(OsStr::new("c"))
            })
            .map(|e| e.into_path())
            .map(|c_file| self.test_file(c_file))
            .filter_map_ok(|it| it)
            .collect::<Result<Vec<_>, _>>()?;
        let o_files = || cc_db_entries.iter().map(|e| e.output.as_ref().unwrap());
        let lib_file = self.dir.join("libtest.a");
        let mut cmd = Command::new("ar");
        cmd.arg("-rv").arg(&lib_file).args(o_files());
        run_command(cmd)?;
        let cc_db_path = self.src_dir.join("compile_commands.json");
        json_compilation_db::to_file(&cc_db_entries, &Default::default(), &cc_db_path)?;
        let config = TranspilerConfig {
            prefix_function_names: Some("rust_".into()),
            overwrite_existing: true,
            log_level: log::LevelFilter::Warn,
            ..Default::default()
        };
        transpile(config, &cc_db_path, &[]);
        let transpiled_files = cc_db_entries.iter().map(|e| e.file.with_extension("rs"));
        let mut mod_names = Vec::new();
        let mut top_level_attributes = Vec::new();
        let mut add_mod = |path: &Path| -> Result<(), AnyError> {
            if path.extension() != Some(OsStr::new("rs")) {
                return Ok(());
            }
            let mut mod_name = match path.file_stem() {
                Some(stem) => stem,
                None => return Ok(()),
            };
            if mod_name == OsStr::new("main") {
                return Ok(());
            }
            let directives = Directives::read(path)?;
            if directives.skip() {
                return Ok(());
            }
            if mod_name == OsStr::new("mod") {
                mod_name = path.parent().unwrap().file_name().unwrap();
            }
            mod_names.push(mod_name.to_string_lossy().into_owned().replace("-", "_"));
            top_level_attributes.extend(directives.top_level_attributes());
            Ok(())
        };
        for dir_entry in fs::read_dir(&self.src_dir)? {
            let dir_entry = dir_entry?;
            if dir_entry.file_type()?.is_dir() {
                let mod_rs = dir_entry.path().join("mod.rs");
                if mod_rs.exists() {
                    add_mod(&mod_rs)?;
                }
            } else {
                add_mod(&dir_entry.path())?;
            }
        }
        let mod_decls = mod_names
            .iter()
            .map(|name| format!("mod {name};"))
            .join("\n");
        let top_level_attributes = top_level_attributes.iter().unique().join("\n");
        let main_rs_contents =
            [top_level_attributes, mod_decls, "fn main() {}".into()].join("\n\n");
        let main_rs_file = self.src_dir.join("main.rs");
        fs::write(&main_rs_file, main_rs_contents)?;
        let mut cmd = Command::new("cargo");
        cmd.arg("test");
        if !cfg!(debug_assertions) {
            cmd.arg("--release");
        }
        dbg!(&self.dir);
        cmd.current_dir(self.dir.as_path());
        run_command(cmd)?;
        if !keep_artifacts {
            fs::remove_file(&main_rs_file)?;
            for transpiled_file in transpiled_files {
                fs::remove_file(transpiled_file)?;
            }
            fs::remove_file(cc_db_path)?;
            fs::remove_file(&lib_file)?;
            for o_file in o_files() {
                fs::remove_file(o_file)?;
            }
        }
        Ok(())
    }
}

fn test_translator_using_rust(to_skip: &[&str]) -> Result<(), AnyError> {
    let to_skip = to_skip.iter().map(OsStr::new).collect::<HashSet<_>>();
    let mut dirs = Vec::new();
    for dir in fs::read_dir("../tests")? {
        let dir = dir?;
        if !dir.file_type()?.is_dir() {
            continue;
        }
        let dir = dir.path();
        // TODO can use let ... && in newer rust
        if let Some(file_name) = dir.file_name() {
            if to_skip.contains(file_name) {
                continue;
            }
        }
        dirs.push(dir);
    }
    // par_iter() causes segfaults and other errors in c2rust
    dirs.iter()
        .map(|dir| -> Result<_, _> { TestDir::new(dir)?.test_dir(false) })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(())
}

fn test_translator_using_python(to_run: &[&str]) -> io::Result<()> {
    let to_run_regex = to_run.iter().join("|");
    let status = Command::new("../scripts/test_translator.py")
        .args(&[
            "../tests/",
            "--only-directories",
            &to_run_regex,
        ])
        .status()?;
    process::exit(status.code().unwrap_or_else(|| match status.success() {
        true => 0,
        false => 1,
    }));
}

#[test]
fn test_translator() -> Result<(), AnyError> {
    // run both in one function so they don't parallelize
    // since c2rust crashes often when run in parallel
    // and both concurrently modify files in the test directories
    let to_skip = &["longdouble", "asm.aarch64", "asm.arm", "comments", "macros"];
    test_translator_using_rust(to_skip)?;
    test_translator_using_python(&[])?;
    Ok(())
}
