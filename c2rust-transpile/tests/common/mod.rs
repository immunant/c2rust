use std::path::PathBuf;

use c2rust_rust_tools::RustEdition;
use c2rust_transpile::{ReplaceMode, TranspilerConfig};

pub fn config(edition: RustEdition) -> TranspilerConfig {
    TranspilerConfig {
        dump_untyped_context: false,
        dump_typed_context: false,
        pretty_typed_context: false,
        dump_function_cfgs: false,
        json_function_cfgs: false,
        dump_cfg_liveness: false,
        dump_structures: false,
        verbose: false,
        debug_ast_exporter: false,
        emit_c_decl_map: true, // Let snapshot tests validate the C declaration map.
        incremental_relooper: true,
        fail_on_multiple: false,
        filter: None,
        debug_relooper_labels: false,
        cross_checks: false,
        cross_check_backend: Default::default(),
        cross_check_configs: Default::default(),
        prefix_function_names: None,
        translate_asm: true,
        use_c_loop_info: true,
        use_c_multiple_info: true,
        simplify_structures: true,
        panic_on_translator_failure: false,
        emit_modules: false,
        fail_on_error: true,
        replace_unsupported_decls: ReplaceMode::Extern,
        translate_valist: true,
        overwrite_existing: true,
        reduce_type_annotations: false,
        reorganize_definitions: false,
        enabled_warnings: Default::default(),
        emit_no_std: false,
        output_dir: None,
        translate_const_macros: Default::default(),
        translate_fn_macros: Default::default(),
        disable_rustfmt: false,
        disable_refactoring: false,
        preserve_unused_functions: false,
        log_level: log::LevelFilter::Warn,
        edition,
        deny_unsafe_op_in_unsafe_fn: false,
        postprocess: false,
        emit_build_files: false,
        binaries: Vec::new(),
        thin_binaries: false,
        c2rust_dir: Some(
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .to_path_buf(),
        ),
    }
}
