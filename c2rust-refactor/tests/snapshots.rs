use c2rust_refactor::file_io::OutputMode;
use c2rust_refactor::lib_main;
use c2rust_refactor::Command as RefactorCommand;
use c2rust_refactor::Options;
use c2rust_refactor::RustcArgSource;
use insta::assert_snapshot;
use std::path::Path;
use std::process::Command;

fn test_refactor_named(command: &str, path: &str) {
    let tests_dir = Path::new("tests/snapshots");
    let old_path = tests_dir.join(path);
    let new_path = old_path.with_extension("new"); // Output from `alongside`.

    // TODO Make sure `c2rust-transpile` and `c2rust-refactor` use the same edition.
    // Refactor it into a `const`.
    let edition = "2021";

    let old_path = old_path.to_str().unwrap();
    let rustc_args = [old_path, "--edition", edition];

    lib_main(Options {
        rewrite_modes: vec![OutputMode::Alongside],
        commands: vec![RefactorCommand {
            name: command.to_owned(),
            args: vec![],
        }],
        rustc_args: RustcArgSource::CmdLine(rustc_args.map(|arg| arg.to_owned()).to_vec()),
        cursors: Default::default(),
        marks: Default::default(),
        plugins: Default::default(),
        plugin_dirs: Default::default(),
    })
    .unwrap();

    // TODO Run `rustfmt` by default as part of `c2rust-refactor`
    // with the same `--disable-rustfmt` flag that `c2rust-transpile` has.
    // Then import `fn rustfmt` from `c2rust_transpile` to do this.
    let status = Command::new("rustfmt")
        .args(["--edition", edition])
        .arg(&new_path)
        .status()
        .unwrap();
    assert!(status.success());

    let new_rs = fs_err::read_to_string(&new_path).unwrap();

    let snapshot_name = if Some(command) == path.strip_suffix(".rs") {
        format!("refactor-{path}")
    } else {
        format!("refactor-{command}-{path}")
    };
    let rustc_args = shlex::try_join(rustc_args).unwrap();
    let debug_expr = format!("c2rust-refactor {command} --rewrite-mode alongside -- {rustc_args}");

    assert_snapshot!(snapshot_name, new_rs, &debug_expr);
}

fn test_refactor(command: &str) {
    test_refactor_named(command, &format!("{command}.rs"));
}

// NOTE: Tests should be listed in alphabetical order.

#[test]
fn test_convert_exits() {
    test_refactor("convert_exits");
}

#[test]
fn test_convert_exits_skip() {
    test_refactor_named("convert_exits", "convert_exits_skip.rs");
}

#[test]
fn test_convert_math_funcs() {
    test_refactor("convert_math_funcs");
}

#[test]
fn test_convert_math_skip() {
    test_refactor_named("convert_math_funcs", "convert_math_skip.rs");
}

#[test]
fn test_fix_unused_unsafe() {
    test_refactor("fix_unused_unsafe");
}

#[test]
fn test_fold_let_assign() {
    test_refactor("fold_let_assign");
}

#[test]
fn test_let_x_uninitialized() {
    test_refactor("let_x_uninitialized");
}

#[test]
fn test_reconstruct_while() {
    test_refactor("reconstruct_while");
}

/// TODO Broken
/// Suffixes are not actually removed.
#[test]
fn test_remove_literal_suffixes() {
    test_refactor("remove_literal_suffixes");
}

#[test]
fn test_remove_unused_labels() {
    test_refactor("remove_unused_labels");
}

#[test]
fn test_rename_unnamed() {
    test_refactor("rename_unnamed");
}

#[test]
fn test_reorder_derives() {
    test_refactor_named("noop", "reorder_derives.rs");
}

#[cfg(target_os = "linux")] // `statvfs` and `statfs64` are Linux only.
#[test]
fn test_reorganize_definitions() {
    test_refactor("reorganize_definitions");
}

#[test]
fn test_sink_lets() {
    test_refactor("sink_lets");
}

#[test]
fn test_struct_assign_to_update() {
    test_refactor("struct_assign_to_update");
}

#[test]
fn test_struct_merge_updates() {
    test_refactor("struct_merge_updates");
}

/// TODO Broken
/// `f(x)` doesn't become `x + 1`.
#[test]
fn test_test_f_plus_one() {
    test_refactor("test_f_plus_one");
}

/// TODO Broken
/// `2` doesn't become `1 + 1`.
#[test]
fn test_test_one_plus_one() {
    test_refactor("test_one_plus_one");
}

#[test]
fn test_test_reflect() {
    test_refactor("test_reflect");
}

#[test]
fn test_uninit_to_default() {
    test_refactor("uninit_to_default");
}
