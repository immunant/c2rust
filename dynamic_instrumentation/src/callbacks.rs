use once_cell::sync::Lazy;
use rustc_middle::util::Providers;
use rustc_session::config::{ExternEntry, ExternLocation, Externs};
use rustc_session::Session;
use rustc_span::def_id::LocalDefId;

use crate::instrument::Instrumenter;

pub static INSTRUMENTER: Lazy<Instrumenter> = Lazy::new(Instrumenter::new);

pub struct MirTransformCallbacks;

impl rustc_driver::Callbacks for MirTransformCallbacks {
    fn config(&mut self, config: &mut rustc_interface::Config) {
        config.opts.incremental = None;
        config.override_queries = Some(override_queries);

        // The compiler's MIR validator is now private. Preserve mandatory
        // validation via the pass manager: this checks all bodies downstream
        // after compiler passes, rather than just our body immediately here.
        // It is broader and potentially slower, but must remain enabled even
        // when the caller disables the compiler's optional validation.
        config.opts.unstable_opts.validate_mir = true;

        // Loading a forced extern resolves it even when source code never names
        // it. This replaces the injected `extern crate` AST item and makes the
        // runtime hooks available to MIR instrumentation in every edition.
        let mut externs = config
            .opts
            .externs
            .iter()
            .map(|(name, entry)| (name.clone(), entry.clone()))
            .collect::<std::collections::BTreeMap<_, _>>();
        externs
            .entry("c2rust_analysis_rt".to_owned())
            .or_insert(ExternEntry {
                location: ExternLocation::FoundInLibrarySearchDirectories,
                is_private_dep: false,
                add_prelude: true,
                nounused_dep: true,
                force: true,
            })
            .force = true;
        config.opts.externs = Externs::new(externs);
    }
}

fn override_queries(_sess: &Session, providers: &mut Providers) {
    providers.mir_built = |tcx, def: LocalDefId| {
        let mut providers = Providers::default();
        rustc_mir_transform::provide(&mut providers);

        let steal_mir = (providers.mir_built)(tcx, def);
        let mut mir = steal_mir.steal();

        let body_did = def.to_def_id();
        let fn_ty = tcx.type_of(body_did).instantiate_identity();
        if fn_ty.is_fn() && !tcx.is_const_fn(body_did) && !tcx.is_static(body_did) {
            INSTRUMENTER.instrument_fn(tcx, &mut mir, body_did);
        }

        tcx.alloc_steal_mir(mir)
    };
}
