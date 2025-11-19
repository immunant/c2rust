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
        translate_const_macros: Default::default(),
        translate_fn_macros: Default::default(),
        disable_refactoring: false,
        preserve_unused_functions: false,
        log_level: log::LevelFilter::Warn,
        emit_build_files: false,
        binaries: Vec::new(),
    }
}

/// `platform` can be any platform-specific string.
/// It could be the `target_arch`, `target_os`, some combination, or something else.
fn transpile(platform: Option<&str>, c_path: &Path) {
    let status = Command::new("clang")
        .args([
            "-c",
            "-o",
            "/dev/null",
            "-w", // Disable warnings.
        ])
        .arg(c_path)
        .status();
    assert!(status.unwrap().success());

    let (_temp_dir, temp_path) =
        c2rust_transpile::create_temp_compile_commands(&[c_path.to_owned()]);
    c2rust_transpile::transpile(
        config(),
        &temp_path,
        &[
            "-w", // Disable warnings.
        ],
    );
    let cwd = current_dir().unwrap();
    let c_path = c_path.strip_prefix(&cwd).unwrap();
    // The crate name can't have `.`s in it, so use the file stem.
    // This is also why we set it explicitly with `--crate-name`,
    // as once we add `.{platform}`, the crate name derived from
    // the file name won't be valid anymore.
    let crate_name = c_path.file_stem().unwrap().to_str().unwrap();
    let rs_path = c_path.with_extension("rs");
    // We need to move the `.rs` file to a platform-specific name
    // so that they don't overwrite each other.
    let rs_path = match platform {
        None => rs_path,
        Some(platform) => {
            let platform_rs_path = rs_path.with_extension(format!("{platform}.rs"));
            fs::rename(&rs_path, &platform_rs_path).unwrap();
            platform_rs_path
        }
    };

    let edition = "2021";

    let status = Command::new("rustfmt")
        .args(["--edition", edition])
        .arg(&rs_path)
        .status();
    assert!(status.unwrap().success());

    let rs = fs::read_to_string(&rs_path).unwrap();
    let debug_expr = format!("cat {}", rs_path.display());

    let snapshot_name = match platform {
        None => "transpile".into(),
        Some(platform) => format!("transpile-{platform}"),
    };
    insta::assert_snapshot!(snapshot_name, &rs, &debug_expr);

    // Using rustc itself to build snapshots that reference libc is difficult because we don't know
    // the appropriate --extern libc=/path/to/liblibc-XXXXXXXXXXXXXXXX.rlib to pass. Skip for now,
    // as we've already compared the literal text.
    if rs.contains("libc::") {
        eprintln!(
            "warning: skipping compiling {} with rustc since it depends on libc",
            rs_path.display()
        );
        return;
    }

    // Don't need to worry about platform clashes here, as this is immediately deleted.
    let rlib_path = format!("lib{crate_name}.rlib");
    let status = Command::new("rustc")
        .args([
            "+nightly-2023-04-15",
            "--crate-type",
            "lib",
            "--edition",
            edition,
            "--crate-name",
            crate_name,
            "-o",
            &rlib_path,
            "-Awarnings", // Disable warnings.
        ])
        .arg(&rs_path)
        .status();
    assert!(status.unwrap().success());
    fs::remove_file(&rlib_path).unwrap();
}

#[test]
fn transpile_all() {
    insta::glob!("snapshots/*.c", |x| transpile(None, x));

    // Some things transpile differently on Linux vs. macOS,
    // as they use `unsigned long` and `unsigned long long` differently for builtins.
    // This makes snapshot tests trickier, as the output will be OS-dependent.
    // We handle this by adding OS name to the snapshot result filename.
    #[allow(unused)]
    let os = "unknown";

    #[cfg(target_os = "linux")]
    let os = "linux";
    #[cfg(target_os = "macos")]
    let os = "macos";

    // Similarly, some things transpile differently on different architectures.
    #[allow(unused)]
    let arch = "unknown";

    #[cfg(target_arch = "x86")]
    let arch = "x86";
    #[cfg(target_arch = "x86_64")]
    let arch = "x86_64";
    #[cfg(target_arch = "arm")]
    let arch = "arm";
    #[cfg(target_arch = "aarch64")]
    let arch = "aarch64";

    let arch_os = format!("{}-{}", arch, os);

    insta::with_settings!({snapshot_suffix => os}, {
        insta::glob!("snapshots/os-specific/*.c", |path| transpile(Some(os), path));
    });

    insta::with_settings!({snapshot_suffix => arch}, {
        insta::glob!("snapshots/arch-specific/*.c", |path| transpile(Some(arch), path));
    });

    insta::with_settings!({snapshot_suffix => arch_os.as_str()}, {
        insta::glob!("snapshots/arch-os-specific/*.c", |path| transpile(Some(arch_os.as_str()), path));
    });
}
