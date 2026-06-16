use std::fs;
use std::path::PathBuf;
use std::process::Command;

use c2rust_rust_tools::RustEdition;
use c2rust_transpile::{ReplaceMode, TranspilerConfig};
use tempfile::TempDir;

fn config(output_dir: PathBuf) -> TranspilerConfig {
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
        emit_c_decl_map: false,
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
        emit_modules: true,
        fail_on_error: true,
        replace_unsupported_decls: ReplaceMode::Extern,
        translate_valist: true,
        overwrite_existing: true,
        reduce_type_annotations: false,
        reorganize_definitions: false,
        enabled_warnings: Default::default(),
        emit_no_std: false,
        output_dir: Some(output_dir),
        translate_const_macros: Default::default(),
        translate_fn_macros: Default::default(),
        disable_rustfmt: false,
        disable_refactoring: false,
        preserve_unused_functions: false,
        log_level: log::LevelFilter::Warn,
        edition: RustEdition::Edition2021,
        deny_unsafe_op_in_unsafe_fn: false,
        postprocess: false,
        emit_build_files: true,
        binaries: vec!["main".to_owned()],
        thin_binaries: true,
        c2rust_dir: Some(
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .to_path_buf(),
        ),
    }
}

#[test]
fn sibling_file_and_directory_modules_do_not_collide() {
    let td = TempDir::new().unwrap();
    let src = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/module_paths/sibling_file_and_directory");
    let output_dir = td.path().join("translated");

    let (_compile_commands_dir, compile_commands_path) =
        c2rust_transpile::create_temp_compile_commands(&[
            src.join("main.c"),
            src.join("hash.c"),
            src.join("hash/sha1.c"),
        ]);

    c2rust_transpile::transpile(config(output_dir.clone()), &compile_commands_path, &["-w"]);

    let lib_rs = fs::read_to_string(output_dir.join("lib.rs")).unwrap();
    assert!(
        lib_rs.contains("#[path = \"src/hash.rs\"]\npub mod c2rust_src_hash;"),
        "generated module tree should include the sibling hash.rs module via an \
         explicit path module instead of shadowing it with the hash/ directory:\n{lib_rs}"
    );
    assert!(
        lib_rs.contains("pub mod sha1;"),
        "generated lib.rs should include the nested hash/sha1.rs module:\n{lib_rs}"
    );

    let status = Command::new("cargo")
        .arg("build")
        .env("CARGO_TARGET_DIR", td.path().join("target"))
        .current_dir(&output_dir)
        .status()
        .unwrap();
    assert!(status.success(), "generated crate should build");
}
