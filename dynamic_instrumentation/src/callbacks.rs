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

        // The compiler's MIR validator is private. Enable per-pass validation
        // so instrumented MIR is checked during `cargo check` too, which never
        // reaches the unconditional validation at Runtime(Optimized).
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

#[cfg(test)]
mod tests {
    use super::*;
    use rustc_driver::{Callbacks, RunCompiler};
    use std::process::Command;

    struct InvalidMirCallbacks;

    impl Callbacks for InvalidMirCallbacks {
        fn config(&mut self, config: &mut rustc_interface::Config) {
            MirTransformCallbacks.config(config);
            // Exercise the production session settings with a deliberately
            // broken MIR transformation that needs no runtime hooks.
            config.opts.externs = Externs::new(Default::default());
            config.override_queries = Some(|_, providers| {
                providers.mir_built = |tcx, def| {
                    let mut providers = Providers::default();
                    rustc_mir_transform::provide(&mut providers);
                    let mut body = (providers.mir_built)(tcx, def).steal();
                    body.local_decls[rustc_middle::mir::RETURN_PLACE].ty = tcx.types.i32;
                    tcx.alloc_steal_mir(body)
                };
            });
        }
    }

    #[test]
    fn check_rejects_invalid_transformed_mir() {
        const CHILD_DIR: &str = "C2RUST_TEST_INVALID_MIR_DIR";
        if let Some(dir) = std::env::var_os(CHILD_DIR) {
            let dir = std::path::PathBuf::from(dir);
            let sysroot = Command::new("rustc")
                .args(["--print", "sysroot"])
                .output()
                .unwrap();
            assert!(sysroot.status.success());
            RunCompiler::new(
                &[
                    "rustc".into(),
                    dir.join("invalid.rs").to_str().unwrap().into(),
                    "--sysroot".into(),
                    String::from_utf8(sysroot.stdout).unwrap().trim().into(),
                    "--crate-type=lib".into(),
                    "--emit=metadata".into(),
                    "--out-dir".into(),
                    dir.to_str().unwrap().into(),
                    "-Zmir-opt-level=0".into(),
                    "-Zvalidate-mir=no".into(),
                ],
                &mut InvalidMirCallbacks,
            )
            .run();
            return;
        }
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("invalid.rs"),
            "pub fn value() -> bool { true }",
        )
        .unwrap();
        let output = Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "callbacks::tests::check_rejects_invalid_transformed_mir",
                "--nocapture",
            ])
            .env(CHILD_DIR, dir.path())
            .current_dir(dir.path())
            .output()
            .unwrap();
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success(), "invalid MIR passed cargo check");
        assert!(stderr.contains("broken MIR"), "{stderr}");
        assert!(stderr.contains("after pass"), "{stderr}");
    }
}
