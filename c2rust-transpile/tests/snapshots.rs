use std::env::current_dir;
use std::fs;
use std::path::Path;
use std::process::Command;

use c2rust_transpile::{ReplaceMode, TranspilerConfig};

fn config() -> TranspilerConfig {
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
        fail_on_error: true,
        replace_unsupported_decls: ReplaceMode::Extern,
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
    }
}

fn transpile(c_path: &Path) {
    let parent_dir_name = c_path
        .parent()
        .and_then(|dir| dir.file_name())
        .and_then(|file_name| file_name.to_str())
        .unwrap_or_default();
    // Some things transpile differently on Linux vs. macOS,
    // as they use `unsigned long` and `unsigned long long` differently for builtins.
    // This makes snapshot tests trickier, as the output will be OS-dependent.
    // We only test Linux here, as that should be sufficient for these specific tests,
    // and because cross-compiling with transpilation is not super straightforward,
    // so generating the macOS snapshots locally on Linux is annoying.
    if parent_dir_name == "linux" && !cfg!(target_os = "linux") {
        return;
    }

    let status = Command::new("clang")
        .args(&["-c", "-o", "/dev/null"])
        .arg(c_path)
        .status();
    assert!(status.unwrap().success());

    let (_temp_dir, temp_path) =
        c2rust_transpile::create_temp_compile_commands(&[c_path.to_owned()]);
    c2rust_transpile::transpile(config(), &temp_path, &[]);
    let cwd = current_dir().unwrap();
    let c_path = c_path.strip_prefix(&cwd).unwrap();
    let rs_path = c_path.with_extension("rs");
    let rs = fs::read_to_string(&rs_path).unwrap();
    let debug_expr = format!("cat {}", rs_path.display());
    insta::assert_snapshot!("transpile", &rs, &debug_expr);

    let status = Command::new("rustc")
        .args(&["--crate-type", "lib", "--edition", "2021", "-o", "-"])
        .arg(&rs_path)
        .status();
    assert!(status.unwrap().success());
}

#[test]
fn transpile_all() {
    // We need to do this as a single glob,
    // as `insta` removes the common prefix to all matches files,
    // and if we do this as separate globs (for linux-only files),
    // they'll overwrite each other.
    insta::glob!("snapshots/**/*.c", transpile);
}
