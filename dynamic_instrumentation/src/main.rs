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

mod instrument_memory;
use instrument_memory::InstrumentMemoryOps;

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
use std::env;
use std::ffi::OsStr;
use std::path::Path;

lazy_static! {
    static ref INSTRUMENTER: InstrumentMemoryOps = InstrumentMemoryOps::new();
    static ref METADATA_FILE_PATH: String = {
        env::args()
            .skip(1)
            .next()
            .expect("Expected metadata ouput file path as the first argument")
    };
}

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

        // Get the name of the function we're compiling.
        let name = tcx.item_name(def.did.to_def_id());
        dbg!(name);

        INSTRUMENTER.instrument_fn(tcx, &mut mir, def.did.to_def_id());
        dbg!(&mir);

        validate::Validator {
            when: "After dynamic instrumentation".to_string(),
            mir_phase: mir.phase,
        }
        .run_pass(tcx, &mut mir);

        tcx.alloc_steal_mir(mir)
    };
}

fn main() -> anyhow::Result<()> {
    // skip the first arg, which is the path to this tool. This leaves the
    // metadata file as the "path" to the tool, but that's fine since it's
    // ignored.
    let args = env::args().skip(1).collect::<Vec<_>>();
    let mut callbacks = MirTransformCallbacks;
    rustc_driver::RunCompiler::new(&args, &mut callbacks)
        .run()
        .or_else(|_| Err(anyhow!("Compilation failed")))?;
    let file_path: &OsStr = METADATA_FILE_PATH.as_ref();
    INSTRUMENTER.finalize(&Path::new(file_path))
}
