use once_cell::sync::Lazy;
use rustc_ast::ast::{Item, ItemKind, Visibility, VisibilityKind};
use rustc_ast::node_id::NodeId;
use rustc_ast::ptr::P;
use rustc_const_eval::transform::validate::Validator;
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

use thin_vec::ThinVec;
use crate::instrument::Instrumenter;

pub static INSTRUMENTER: Lazy<Instrumenter> = Lazy::new(Instrumenter::new);

pub struct MirTransformCallbacks;

impl rustc_driver::Callbacks for MirTransformCallbacks {
    fn config(&mut self, config: &mut rustc_interface::Config) {
        config.opts.incremental = None;
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
            attrs: ThinVec::new(),
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

            Validator {
                when: "After dynamic instrumentation".to_string(),
                mir_phase: mir.phase,
            }
            .run_pass(tcx, &mut mir);
        }

        tcx.alloc_steal_mir(mir)
    };
}
