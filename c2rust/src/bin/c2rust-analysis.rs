extern crate c2rust_refactor;
extern crate cargo;
#[macro_use]
extern crate clap;
extern crate tempfile;
extern crate toml_edit;

use std::env;
use std::fs;
use std::io;
use std::os::unix;
use std::path::{Path, PathBuf};

use cargo::{Config, CargoResult};
use cargo::core::{shell, Shell, Workspace};
use cargo::core::compiler::CompileMode;
use cargo::ops::{CompileOptions, compile};
use cargo::util::config;
use cargo::util::important_paths::find_root_manifest_for_wd;
use clap::{App, ArgMatches};
use tempfile::{tempdir, TempDir};

fn main() {
    let yaml = load_yaml!("../analysis.yaml");
    let matches = App::from_yaml(yaml)
        .get_matches();

    match matches.subcommand() {
        ("instrument", Some(sub_matches)) => Instrumenter::new(sub_matches).run(), 
        ("process", Some(sub_matches)) => handle_process(sub_matches),
       _ => (),
    };
}

pub const SPAN_FILENAME: &str = "lifetime_analysis_spans.bincode";

struct AnalysisContext {
    src_path: PathBuf,
    target_dir: PathBuf,
}

impl AnalysisContext {
    fn new() -> Self {
        let src_config = Config::default().unwrap();
        let src_manifest_file = find_root_manifest_for_wd(src_config.cwd()).unwrap();
        let src_path = src_manifest_file.parent().unwrap().to_owned();

        let src_ws = Workspace::new(&src_manifest_file, &src_config).unwrap();
        let target_dir = src_ws.target_dir().join("instrumented").into_path_unlocked();

        Self {
            src_path,
            target_dir,
        }
    }

    fn span_path(&self) -> PathBuf {
        self.target_dir.join(SPAN_FILENAME)
    }
}

struct Instrumenter {
    ctx: AnalysisContext,
    _out_tempdir: Option<TempDir>,
    out_config: Config,
    out_path: PathBuf,
    main_path: String,
}

impl Instrumenter {
    fn new(matches: &ArgMatches) -> Self {
        let ctx = AnalysisContext::new();

        let mut out_tempdir = Some(tempdir().expect("Could not create temporary directory"));
        let out_path = {
            if matches.is_present("keep-temps") {
                out_tempdir.take().unwrap().into_path()
            } else {
                out_tempdir.as_ref().unwrap().path().to_owned()
            }
        };

        fs::create_dir_all(&ctx.target_dir)
            .expect(&format!("Could not create target directory: {:?}", &ctx.target_dir));

        let mut shell = Shell::new();
        shell.set_verbosity(shell::Verbosity::Quiet);
        let out_config = Config::new(
            shell,
            out_path.clone(),
            config::homedir(&out_path).unwrap()
        );
        let out_target_dir = out_config
            .target_dir()
            .unwrap() // Unwrap the CargoResult
            .map(|fs| fs.into_path_unlocked()) // Filesystem -> PathBuf
            .unwrap_or(out_path.join("target")); // default
        fs::create_dir_all(&out_target_dir)
            .expect(&format!(
                "Could not create output target directory {}",
                out_target_dir.to_string_lossy()
            ));
        let out_target_dir = out_target_dir.join("release");
        unix::fs::symlink(&ctx.target_dir, &out_target_dir)
            .expect(&format!(
                "Could not create output target directory symlink from {} to {}",
                out_target_dir.to_string_lossy(),
                ctx.target_dir.to_string_lossy(),
            ));

        if matches.is_present("keep-temps") {
            println!("Saving instrumented crate to: {}", out_path.to_str().unwrap());
        }

        Self {
            ctx,
            _out_tempdir: out_tempdir,
            out_config,
            out_path,
            main_path: matches.value_of_lossy("main").unwrap().to_string(),
        }
    }

    fn run(&self) {
        copy_recursively(&self.ctx.src_path, &self.out_path)
            .expect("Error copying source files to temporary crate build directory");

        self.run_instrumentation_refactoring();
        self.add_instrumentation_dependency()
            .expect("Could not add dependency on instrumentaton crate");

        self.build()
            .expect("Could not build instrumented crate");
    }

    fn run_instrumentation_refactoring(&self) {
        env::set_current_dir(&self.out_path)
            .expect("Could not change to temporary build directory");
        let refactor_options = c2rust_refactor::Options {
            rewrite_modes: vec![c2rust_refactor::file_io::OutputMode::InPlace],
            commands: vec![
                c2rust_refactor::Command {
                    name: String::from("lifetime_analysis_instrument"),
                    args: vec![
                        String::from(self.ctx.span_path().to_str().unwrap()),
                        self.main_path.clone(),
                    ],
                },
            ],
            rustc_args: c2rust_refactor::RustcArgSource::Cargo,
            cursors: vec![],
            marks: vec![],
            plugins: vec![],
            plugin_dirs: vec![],
        };

        c2rust_refactor::lib_main(refactor_options)
            .expect("Refactoring instrumentation pass did not run correctly");
    }

    fn add_instrumentation_dependency(&self) -> io::Result<()> {
        let manifest_path = find_root_manifest_for_wd(&self.out_path).unwrap();
        let manifest_str = fs::read_to_string(&manifest_path)?;
        let mut manifest: toml_edit::Document = manifest_str.parse()
            .expect("Could not parse manifest");
        manifest["dependencies"]["c2rust-analysis-rt"]["path"] = toml_edit::value("/home/sjcrane/projects/c2rust/src/analysis/runtime/");
        fs::write(&manifest_path, manifest.to_string())?;
        Ok(())
    }

    /// Returns the target directory containing the built crate
    fn build(&self) -> CargoResult<()> {
        let mut compile_opts = CompileOptions::new(&self.out_config, CompileMode::Build)?;
        compile_opts.build_config.release = true;

        let manifest_file = find_root_manifest_for_wd(self.out_config.cwd())?;

        let ws = Workspace::new(&manifest_file, &self.out_config)?;
        compile(&ws, &compile_opts)?;

        Ok(())
    }
}

fn copy_recursively(src: &Path, dest: &Path) -> io::Result<()> {
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let src_path = entry.path();
        let dest_path = dest.join(entry.file_name());
        match entry.metadata()?.file_type() {
            file_type if file_type.is_dir() => {
                if !dest_path.is_dir() {
                    fs::create_dir(&dest_path)?;
                }
                copy_recursively(&src_path, &dest_path)?;
            }
            file_type if file_type.is_file() => {
                fs::copy(&src_path, &dest_path)?;
            }
            file_type if file_type.is_symlink() => {
                unimplemented!("Copying symlinks is not implemented yet");
            }
            _ => panic!("Unexpected file type"),
        }
    }
    Ok(())
}

fn handle_process(matches: &ArgMatches) {
    let ctx = AnalysisContext::new();

    let span_path = String::from(ctx.target_dir.join(SPAN_FILENAME).canonicalize().unwrap().to_string_lossy());

    let log_path = Path::new(matches.value_of_os("log").unwrap());
    let log_path = String::from(log_path.canonicalize().unwrap().to_string_lossy());

    let refactor_options = c2rust_refactor::Options {
        rewrite_modes: vec![c2rust_refactor::file_io::OutputMode::PrintDiff],
        commands: vec![
            c2rust_refactor::Command {
                name: String::from("lifetime_analysis"),
                args: vec![span_path, log_path],
            },
        ],
        rustc_args: c2rust_refactor::RustcArgSource::Cargo,
        cursors: vec![],
        marks: vec![],
        plugins: vec![],
        plugin_dirs: vec![],
    };

    c2rust_refactor::lib_main(refactor_options)
        .expect("Refactoring instrumentation pass did not run correctly");
}
