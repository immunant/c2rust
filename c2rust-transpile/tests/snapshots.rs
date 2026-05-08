use std::env::current_dir;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use c2rust_rust_tools::rustc;
use c2rust_rust_tools::sanitize_file_name;
use c2rust_rust_tools::RustEdition;
use c2rust_rust_tools::RustEdition::Edition2021;
use c2rust_rust_tools::RustEdition::Edition2024;
use c2rust_transpile::renamer::RUST_KEYWORDS;
use c2rust_transpile::ReplaceMode;
use c2rust_transpile::TranspilerConfig;
use itertools::Itertools;

fn config(edition: RustEdition) -> TranspilerConfig {
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
        emit_c_decl_map: true, // So `transpile_with_c_decl_map_snapshot` can validate C decl map.
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

/// Validate that the given C file compiles, then transpile it with the given config.
fn compile_and_transpile_file(c_path: &Path, config: TranspilerConfig) {
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
        config,
        &temp_path,
        &[
            "-w", // Disable warnings.
        ],
    );
}

/// Transpile one input and compare output against the corresponding snapshot.
/// For outputs that vary in different environments,
/// `platform` should be a slice of the platform-specific parts,
/// such as the `target_arch` or  `target_os` or both.
fn transpile_snapshot(
    platform: &[&str],
    c_path: &Path,
    edition: RustEdition,
    expect_compile_error: bool,
    imported_crates: &[&str],
) {
    let cfg = config(edition);
    compile_and_transpile_file(c_path, cfg);
    let cwd = current_dir().unwrap();
    // The crate name can't have `.`s in it, so use the file stem.
    // This is also why we set it explicitly with `--crate-name`,
    // as once we add `.{platform}`, the crate name derived from
    // the file name won't be valid anymore.
    let crate_name = c_path.file_stem().unwrap().to_str().unwrap();
    let rs_path = c_path.with_extension("rs");
    // We need to move the `.rs` file to a platform/edition-specific name
    // so that they don't overwrite each other.
    let ext = [&[edition.as_str()][..], platform]
        .into_iter()
        .flatten()
        .join(".");
    let old_rs_path = rs_path;
    let rs_path = old_rs_path.with_extension(format!("{ext}.rs"));
    fs::rename(&old_rs_path, &rs_path).unwrap();

    let rs = fs::read_to_string(&rs_path).unwrap();
    let debug_expr = format!("cat {}", rs_path.display());

    // Replace real paths with placeholders
    let rs = rs.replace(cwd.to_str().unwrap(), ".");

    let c_file_name = c_path.file_name().unwrap().to_str().unwrap();
    let c_file_name = sanitize_file_name(&c_file_name);
    let snapshot_name = format!("transpile@{c_file_name}.{ext}");

    insta::assert_snapshot!(snapshot_name, &rs, &debug_expr);

    rustc(&rs_path)
        .edition(edition)
        .crate_name(crate_name)
        .expect_error(expect_compile_error)
        .expect_unresolved_imports(imported_crates)
        .run();
}

#[must_use]
struct TranspileTest<'a> {
    c_file_name: &'a str,
    arch_specific: bool,
    os_specific: bool,
    expect_compile_error_edition_2021: bool,
    expect_compile_error_edition_2024: bool,
    imported_crates: Vec<&'a str>,
}

fn transpile(c_file_name: &str) -> TranspileTest {
    TranspileTest {
        c_file_name,
        arch_specific: false,
        os_specific: false,
        expect_compile_error_edition_2021: false,
        expect_compile_error_edition_2024: false,
        imported_crates: Default::default(),
    }
}

impl<'a> TranspileTest<'a> {
    pub fn arch_specific(self, arch_specific: bool) -> Self {
        Self {
            arch_specific,
            ..self
        }
    }

    pub fn os_specific(self, os_specific: bool) -> Self {
        Self {
            os_specific,
            ..self
        }
    }

    pub fn expect_compile_error_edition_2021(
        self,
        expect_compile_error_edition_2021: bool,
    ) -> Self {
        Self {
            expect_compile_error_edition_2021,
            ..self
        }
    }

    pub fn expect_compile_error_edition_2024(
        self,
        expect_compile_error_edition_2024: bool,
    ) -> Self {
        Self {
            expect_compile_error_edition_2024,
            ..self
        }
    }

    #[allow(unused)] // TODO remove once used
    pub fn expect_compile_error(self, expect_error: bool) -> Self {
        self.expect_compile_error_edition_2021(expect_error)
            .expect_compile_error_edition_2024(expect_error)
    }

    pub fn expect_unresolved_import(mut self, imported_crate: &'a str) -> Self {
        self.imported_crates.push(imported_crate);
        self
    }

    pub fn run(self) {
        let Self {
            c_file_name,
            arch_specific,
            os_specific,
            expect_compile_error_edition_2021,
            expect_compile_error_edition_2024,
            imported_crates,
        } = self;

        let specific_dir_prefix = [arch_specific.then_some("arch"), os_specific.then_some("os")]
            .into_iter()
            .flatten()
            .join("-");
        let c_path = {
            let mut path = Path::new("tests/snapshots").to_owned();
            if !specific_dir_prefix.is_empty() {
                path.push(format!("{specific_dir_prefix}-specific"))
            }
            path.push(c_file_name);
            path
        };

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

        let platform = [arch_specific.then_some(arch), os_specific.then_some(os)]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();

        transpile_snapshot(
            &platform,
            &c_path,
            Edition2021,
            expect_compile_error_edition_2021,
            &imported_crates,
        );
        transpile_snapshot(
            &platform,
            &c_path,
            Edition2024,
            expect_compile_error_edition_2024,
            &imported_crates,
        );
    }
}

fn generate_keywords_test() {
    // Common keywords we need to filter out.
    let c_keywords = [
        "break", "const", "continue", "else", "enum", "extern", "for", "if", "return", "static",
        "struct", "while", "do", "typeof", "char",
    ];
    let mut c_code = RUST_KEYWORDS
        .into_iter()
        .filter(|keyword| !c_keywords.contains(keyword))
        .map(|name| format!("void {name}(void) {{}}"))
        .join("\n\n");
    c_code.push_str("\n");
    let c_path = Path::new("tests/snapshots/keywords.c");
    fs_err::write(c_path, c_code).unwrap();
}

// NOTE: Tests should be listed in alphabetical order.

#[test]
fn test_alloca() {
    transpile("alloca.c").run();
}

#[test]
fn test_arrays() {
    transpile("arrays.c").run();
}

#[test]
fn test_atomics() {
    transpile("atomics.c").run();
}

#[test]
fn test_bitfields() {
    transpile("bitfields.c").expect_compile_error(true).run();
}

#[test]
fn test_bool() {
    transpile("bool.c").run();
}

#[test]
fn test_compound_literals() {
    transpile("compound_literals.c").run();
}

#[test]
fn test_empty_init() {
    transpile("empty_init.c").run();
}

#[test]
fn test_exprs() {
    transpile("exprs.c").run();
}

#[test]
fn test_factorial() {
    transpile("factorial.c").run();
}

#[test]
fn test_fn_attrs() {
    transpile("fn_attrs.c").run();
}

#[test]
fn test_gotos() {
    transpile("gotos.c").run();
}

#[test]
fn test_incomplete_arrays() {
    transpile("incomplete_arrays.c").run();
}

#[test]
fn test_insertion() {
    transpile("insertion.c").run();
}

#[test]
fn test_keywords() {
    generate_keywords_test();
    transpile("keywords.c").run();
}

#[test]
fn test_lift_const() {
    transpile("lift_const.c").run();
}

#[test]
fn test_macrocase() {
    transpile("macrocase.c").run();
}

#[test]
fn test_macros() {
    transpile("macros.c").run();
}

#[test]
fn test_main_fn() {
    transpile("main_fn.c").run();
}

#[test]
fn test_predefined() {
    transpile("predefined.c").run();
}

#[test]
fn test_records() {
    transpile("records.c").run();
}

#[test]
fn test_ref_ub() {
    transpile("ref_ub.c").run();
}

#[test]
fn test_rotate() {
    transpile("rotate.c").run();
}

#[test]
fn test_scalar_init() {
    transpile("scalar_init.c")
        .expect_unresolved_import("f128")
        .run();
}

#[test]
fn test_static_assert() {
    transpile("static_assert.c").run();
}

#[test]
fn test_frame_address() {
    transpile("frame_address.c").run();
}

#[test]
fn test_return_address() {
    transpile("return_address.c").run();
}

#[test]
fn test_return_addr_helpers() {
    transpile("return_addr_helpers.c").run();
}

#[test]
fn test_str_init() {
    transpile("str_init.c").run();
}

#[test]
fn test_volatile() {
    transpile("volatile.c").run();
}

// arch-specific

#[test]
fn test_asm() {
    #[cfg(target_arch = "x86_64")]
    transpile("asm.c")
        .arch_specific(true)
        .expect_unresolved_import("c2rust_asm_casts")
        .run();
    #[cfg(not(target_arch = "x86_64"))]
    transpile("asm.c").arch_specific(true).run();
}

#[test]
fn test_spin() {
    transpile("spin.c").arch_specific(true).run();
}

#[test]
fn test_vm_x86() {
    transpile("vm_x86.c").arch_specific(true).run();
}

// os-specific

#[test]
fn test_call_only_once() {
    transpile("call_only_once.c").os_specific(true).run();
}

#[test]
fn test_f128() {
    transpile("f128.c")
        .expect_unresolved_import("f128")
        .expect_unresolved_import("num_traits")
        .os_specific(true)
        .run();
}

#[test]
fn test_macros_os_specific() {
    transpile("macros.c").os_specific(true).run();
}

#[test]
fn test_out_of_range_lit() {
    transpile("out_of_range_lit.c").os_specific(true).run();
}

#[test]
fn test_rnd() {
    transpile("rnd.c").os_specific(true).run();
}

#[test]
fn test_rotate_os_specific() {
    transpile("rotate.c").os_specific(true).run();
}

#[test]
fn test_sigign() {
    transpile("sigign.c")
        .os_specific(true)
        .expect_unresolved_import("libc")
        .run();
}

#[test]
fn test_typedefidx() {
    transpile("typedefidx.c").os_specific(true).run();
}

#[test]
fn test_types() {
    transpile("types.c")
        .os_specific(true)
        .expect_unresolved_import("libc")
        .run();
}

#[test]
fn test_wide_strings() {
    transpile("wide_strings.c")
        .os_specific(true)
        .expect_unresolved_import("libc")
        .run();
}

// arch-os-specific

#[test]
fn test_varargs() {
    transpile("varargs.c")
        .arch_specific(true)
        .os_specific(true)
        .run();
}

fn transpile_with_c_decl_map_snapshot(c_path: &Path) {
    compile_and_transpile_file(c_path, config(Default::default()));

    let c_decls_path = c_path.with_extension("c_decls.json");
    let snapshot_name = format!("c_decls@{}", c_path.file_name().unwrap().to_str().unwrap());
    let debug_expr = format!("cat {}", c_decls_path.display());
    let json: serde_json::Value =
        serde_json::from_str(&fs::read_to_string(&c_decls_path).unwrap()).unwrap();
    insta::assert_json_snapshot!(snapshot_name, json, &debug_expr);
}

#[test]
fn test_c_decls_nh() {
    let c_path = Path::new("tests/c_decls_snapshots/nh.c");
    transpile_with_c_decl_map_snapshot(c_path);
}
