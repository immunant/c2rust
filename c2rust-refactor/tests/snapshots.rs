use c2rust_refactor::file_io::OutputMode;
use c2rust_refactor::lib_main;
use c2rust_refactor::Command as RefactorCommand;
use c2rust_refactor::Options;
use c2rust_refactor::RustcArgSource;
use insta::assert_snapshot;
use std::path::Path;
use std::process::Command;

fn test_refactor(command: &str) {
    let tests_dir = Path::new("tests");
    let dir = tests_dir.join(command);
    let old_rs_path = dir.join("old.rs");
    let new_path = dir.join("old.new"); // Output from `alongside`.

    // TODO Make sure `c2rust-transpile` and `c2rust-refactor` use the same edition.
    // Refactor it into a `const`.
    let edition = "2021";

    let old_rs_path = old_rs_path.to_str().unwrap();
    let rustc_args = [old_rs_path, "--edition", edition, "-Awarnings"];

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

    let snapshot_name = format!("refactor-{command}");
    let rustc_args = shlex::try_join(rustc_args).unwrap();
    let debug_expr = format!("c2rust-refactor {command} --rewrite-mode alongside -- {rustc_args}");

    assert_snapshot!(snapshot_name, new_rs, &debug_expr);
}

#[test]
fn test_convert_math_funcs() {
    test_refactor("convert_math_funcs");
}

/// TODO Broken
/// `unsafe`s are not actually removed.
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

#[test]
fn test_remove_unused_labels() {
    test_refactor("remove_unused_labels");
}

#[test]
fn test_rename_unnamed() {
    test_refactor("rename_unnamed");
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

#[test]
fn test_uninit_to_default() {
    test_refactor("uninit_to_default");
}
