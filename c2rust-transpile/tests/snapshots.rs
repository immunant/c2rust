use std::fs;

#[test]
fn transpile() {
    let config = || c2rust_transpile::TranspilerConfig {
        dump_untyped_context: false,
        dump_typed_context: false,
        pretty_typed_context: false,
        dump_function_cfgs: false,
        json_function_cfgs: false,
        dump_cfg_liveness: false,
        dump_structures: false,
        verbose: false,
        debug_ast_exporter: false,
        incremental_relooper: true,
        fail_on_multiple: false,
        filter: None,
        debug_relooper_labels: false,
        prefix_function_names: None,
        translate_asm: true,
        use_c_loop_info: true,
        use_c_multiple_info: true,
        simplify_structures: true,
        panic_on_translator_failure: false,
        emit_modules: false,
        fail_on_error: false,
        replace_unsupported_decls: c2rust_transpile::ReplaceMode::Extern,
        translate_valist: true,
        overwrite_existing: true,
        reduce_type_annotations: false,
        reorganize_definitions: false,
        enabled_warnings: Default::default(),
        emit_no_std: false,
        output_dir: None,
        translate_const_macros: false,
        translate_fn_macros: false,
        disable_refactoring: false,
        preserve_unused_functions: false,
        log_level: log::LevelFilter::Warn,
        emit_build_files: false,
        binaries: Vec::new(),
    };
    insta::glob!("snapshots/*.c", |f: &Path| {
        let (_temp_dir, temp_path) =
            c2rust_transpile::create_temp_compile_commands(&[f.to_owned()]);
        c2rust_transpile::transpile(config(), &temp_path, &[]);
        let output_path = f.with_extension("rs");
        let output = fs::read_to_string(&output_path).unwrap();
        fs::remove_file(&output_path).unwrap();
        insta::assert_snapshot!(&output);
    });
}
