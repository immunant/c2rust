#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_driver;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_mir_transform;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_data_structures;

mod instrument_memory;
use instrument_memory::InstrumentMemoryOps;

use cargo::core::compiler::{CompileMode, Context, DefaultExecutor, Executor, Unit};
use cargo::core::{PackageId, Target, Verbosity, Workspace};
use cargo::ops;
use cargo::ops::CompileOptions;
use cargo::util::important_paths::find_root_manifest_for_wd;
use cargo::util::CargoResult;
use cargo::Config;
use cargo_util::ProcessBuilder;

use rustc_ast::ast::{Item, ItemKind, Visibility, VisibilityKind};
use rustc_ast::node_id::NodeId;
use rustc_ast::ptr::P;
use rustc_const_eval::transform::validate;
use rustc_driver::Compilation;
use rustc_interface::interface::Compiler;
use rustc_interface::Queries;
use rustc_middle::mir::MirPass;
use rustc_middle::ty::query::{ExternProviders, Providers};
use rustc_middle::ty::WithOptConstParam;
use rustc_session::Session;
use rustc_span::def_id::LocalDefId;
use rustc_span::symbol::Ident;
use rustc_span::DUMMY_SP;

use anyhow::anyhow;
use lazy_static::lazy_static;
use std::cell::{Cell, RefCell};
use std::env;
use std::ffi::OsString;
use std::io::Write;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

lazy_static! {
    static ref INSTRUMENTER: InstrumentMemoryOps = InstrumentMemoryOps::new();
}

struct NullCallbacks;

impl rustc_driver::Callbacks for NullCallbacks {}

struct MirTransformCallbacks;

impl rustc_driver::Callbacks for MirTransformCallbacks {
    fn config(&mut self, config: &mut rustc_interface::Config) {
        config.override_queries = Some(override_queries);
    }

    fn after_parsing<'tcx>(
        &mut self,
        _compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        let parse = queries.parse().unwrap();
        let mut parse = parse.peek_mut();
        parse.items.push(P(Item {
            attrs: Vec::new(),
            id: NodeId::from_u32(0),
            span: DUMMY_SP,
            vis: Visibility {
                kind: VisibilityKind::Inherited,
                span: DUMMY_SP,
                tokens: None,
            },
            ident: Ident::from_str("c2rust_analysis_rt"),
            kind: ItemKind::ExternCrate(None),
            tokens: None,
        }));
        Compilation::Continue
    }
}

fn override_queries(
    _sess: &Session,
    providers: &mut Providers,
    _extern_providers: &mut ExternProviders,
) {
    providers.mir_built = |tcx, def: WithOptConstParam<LocalDefId>| {
        let mut providers = Providers::default();
        rustc_mir_build::provide(&mut providers);

        let steal_mir = (providers.mir_built)(tcx, def);
        let mut mir = steal_mir.steal();

        let body_did = def.did.to_def_id();
        let fn_ty = tcx.type_of(body_did);
        if fn_ty.is_fn() && !tcx.is_const_fn(body_did) && !tcx.is_static(body_did) {
            INSTRUMENTER.instrument_fn(tcx, &mut mir, body_did);

            validate::Validator {
                when: "After dynamic instrumentation".to_string(),
                mir_phase: mir.phase,
            }
            .run_pass(tcx, &mut mir);
        }

        tcx.alloc_steal_mir(mir)
    };
}

pub fn instrument(
    metadata_file_path: &Path,
    rt_path: &Path,
    args: &[String],
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
    let _ = ops::compile_with_exec(&rt_ws, &compile_opts, &exec_dyn);

    exec.building_rt.store(false, Ordering::Relaxed);
    env::set_current_dir(cwd).unwrap();
    let _ = ops::compile_with_exec(&ws, &compile_opts, &exec_dyn);

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
        if (self.building_rt.load(Ordering::Relaxed) && cx.is_primary_package(unit)) {
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
                .or_else(|_| Err(anyhow!("Compilation failed")))
        } else {
            let mut callbacks = NullCallbacks;
            rustc_driver::RunCompiler::new(&args, &mut callbacks)
                .run()
                .or_else(|_| Err(anyhow!("Compilation failed")))
        }
    }

    fn force_rebuild(&self, unit: &Unit) -> bool {
        self.building_rt.load(Ordering::Relaxed) || self.default.force_rebuild(unit)
    }
}
