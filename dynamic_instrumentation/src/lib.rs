#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_mir_transform;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

mod arg;
mod callbacks;
mod hooks;
mod instrument;
mod into_operand;
mod mir_utils;
mod point;
mod runtime_conversions;
mod util;

use callbacks::{MirTransformCallbacks, NullCallbacks, INSTRUMENTER};

use cargo::core::compiler::{CompileMode, Context, DefaultExecutor, Executor, Unit};
use cargo::core::{PackageId, Target, Verbosity, Workspace};
use cargo::ops;
use cargo::ops::CompileOptions;
use cargo::util::important_paths::find_root_manifest_for_wd;
use cargo::util::CargoResult;
use cargo::Config;
use cargo_util::ProcessBuilder;

use anyhow::anyhow;
use std::env;
use std::ffi::OsString;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

pub fn instrument(
    metadata_file_path: &Path,
    rt_path: &Path,
    _args: &[String],
) -> anyhow::Result<()> {
    let config = Config::default().unwrap();
    config.shell().set_verbosity(Verbosity::Quiet);
    let mode = CompileMode::Build;
    let compile_opts = CompileOptions::new(&config, mode).unwrap();

    let manifest_path = find_root_manifest_for_wd(config.cwd()).unwrap();
    let ws = Workspace::new(&manifest_path, &config).unwrap();

    let rt_manifest_path = find_root_manifest_for_wd(rt_path).unwrap();
    let mut rt_ws = Workspace::new(&rt_manifest_path, &config).unwrap();
    rt_ws.set_target_dir(ws.target_dir());

    let exec = Arc::new(InstrumentationExecutor {
        default: DefaultExecutor,
        target_pkg: ws.current().unwrap().package_id(),
        rt_crate_path: Mutex::new(String::new()),
        building_rt: AtomicBool::new(true),
    });
    let exec_dyn: Arc<dyn Executor> = exec.clone();

    let cwd = env::current_dir().unwrap();

    env::set_current_dir(rt_ws.root()).unwrap();
    ops::compile_with_exec(&rt_ws, &compile_opts, &exec_dyn)?;

    exec.building_rt.store(false, Ordering::Relaxed);
    env::set_current_dir(cwd).unwrap();
    ops::compile_with_exec(&ws, &compile_opts, &exec_dyn)?;

    INSTRUMENTER.finalize(metadata_file_path)
}
struct InstrumentationExecutor {
    default: DefaultExecutor,
    target_pkg: PackageId,
    rt_crate_path: Mutex<String>,
    building_rt: AtomicBool,
}

impl Executor for InstrumentationExecutor {
    fn init(&self, cx: &Context<'_, '_>, unit: &Unit) {
        if self.building_rt.load(Ordering::Relaxed) && cx.is_primary_package(unit) {
            *self.rt_crate_path.lock().unwrap() = cx.outputs(unit).unwrap()[0]
                .path
                .to_str()
                .unwrap()
                .to_owned();
        }
        self.default.init(cx, unit);
    }

    fn exec(
        &self,
        cmd: &ProcessBuilder,
        id: PackageId,
        target: &Target,
        _mode: CompileMode,
        _on_stdout_line: &mut dyn FnMut(&str) -> CargoResult<()>,
        _on_stderr_line: &mut dyn FnMut(&str) -> CargoResult<()>,
    ) -> CargoResult<()> {
        let mut args: Vec<String> = cmd
            .get_args()
            .iter()
            .map(|a| a.to_str().unwrap().to_string())
            .collect();
        args.insert(0, cmd.get_program().to_str().unwrap().to_string());
        // We need to point the rust compiler libraries to the corresponding sysroot
        args.push(format!("--sysroot={}", env!("RUST_SYSROOT")));
        for (var, val) in cmd.get_envs() {
            env::set_var(var, val.as_ref().unwrap_or(&OsString::new()));
        }
        if id == self.target_pkg && !target.for_host() {
            args.push("--extern".to_string());
            args.push(format!(
                "c2rust_analysis_rt={}",
                self.rt_crate_path.lock().unwrap()
            ));
            let mut callbacks = MirTransformCallbacks;
            // TODO: Capture stdout and pass it back to cargo
            rustc_driver::RunCompiler::new(&args, &mut callbacks)
                .run()
                .map_err(|_| anyhow!("Compilation failed"))
        } else {
            let mut callbacks = NullCallbacks;
            rustc_driver::RunCompiler::new(&args, &mut callbacks)
                .run()
                .map_err(|_| anyhow!("Compilation failed"))
        }
    }

    fn force_rebuild(&self, unit: &Unit) -> bool {
        self.building_rt.load(Ordering::Relaxed) || self.default.force_rebuild(unit)
    }
}
