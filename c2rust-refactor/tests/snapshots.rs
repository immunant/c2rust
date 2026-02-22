use c2rust_refactor::file_io::OutputMode;
use c2rust_refactor::lib_main;
use c2rust_refactor::Command as RefactorCommand;
use c2rust_refactor::Options;
use c2rust_refactor::RustcArgSource;
use c2rust_rust_tools::rustfmt;
use c2rust_rust_tools::rustfmt_check;
use c2rust_rust_tools::EDITION;
use insta::assert_snapshot;
use std::path::Path;

struct RefactorTest<'a> {
    command: &'a str,
    path: Option<&'a str>,
}

impl<'a> RefactorTest<'a> {
    pub fn named(self, path: &'a str) -> Self {
        Self {
            path: Some(path),
            ..self
        }
    }

    pub fn test(self) {
        let Self { command, path } = self;
        let path_buf;
        let path = match path {
            Some(path) => path,
            None => {
                path_buf = format!("{command}.rs");
                &path_buf
            }
        };
        test_refactor(command, path);
    }
}

fn refactor(command: &str) -> RefactorTest {
    RefactorTest {
        command,
        path: None,
    }
}

fn test_refactor(command: &str, path: &str) {
    let tests_dir = Path::new("tests/snapshots");
    let old_path = tests_dir.join(path);

    rustfmt_check(&old_path);

    let new_path = old_path.with_extension("new"); // Output from `alongside`.

    let old_path = old_path.to_str().unwrap();
    let rustc_args = [old_path, "--edition", EDITION];

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
    rustfmt(&new_path);

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

// NOTE: Tests should be listed in alphabetical order.

#[test]
fn test_convert_exits() {
    refactor("convert_exits").test();
}

#[test]
fn test_convert_exits_skip() {
    refactor("convert_exits")
        .named("convert_exits_skip.rs")
        .test();
}

#[test]
fn test_convert_math_funcs() {
    refactor("convert_math_funcs").test();
}

#[test]
fn test_convert_math_skip() {
    refactor("convert_math_funcs")
        .named("convert_math_skip.rs")
        .test();
}

#[test]
fn test_fix_unused_unsafe() {
    refactor("fix_unused_unsafe").test();
}

#[test]
fn test_fold_let_assign() {
    refactor("fold_let_assign").test();
}

#[test]
fn test_let_x_uninitialized() {
    refactor("let_x_uninitialized").test();
}

#[test]
fn test_reconstruct_while() {
    refactor("reconstruct_while").test();
}

/// TODO Broken
/// Suffixes are not actually removed.
#[test]
fn test_remove_literal_suffixes() {
    refactor("remove_literal_suffixes").test();
}

#[test]
fn test_remove_unused_labels() {
    refactor("remove_unused_labels").test();
}

#[test]
fn test_rename_unnamed() {
    refactor("rename_unnamed").test();
}

#[test]
fn test_reorder_derives() {
    refactor("noop").named("reorder_derives.rs").test();
}

#[cfg(target_os = "linux")] // `statvfs` and `statfs64` are Linux only.
#[test]
fn test_reorganize_definitions() {
    refactor("reorganize_definitions").test();
}

#[test]
fn test_sink_lets() {
    refactor("sink_lets").test();
}

#[test]
fn test_struct_assign_to_update() {
    refactor("struct_assign_to_update").test();
}

#[test]
fn test_struct_merge_updates() {
    refactor("struct_merge_updates").test();
}

/// TODO Broken
/// `f(x)` doesn't become `x + 1`.
#[test]
fn test_test_f_plus_one() {
    refactor("test_f_plus_one").test();
}

/// TODO Broken
/// `2` doesn't become `1 + 1`.
#[test]
fn test_test_one_plus_one() {
    refactor("test_one_plus_one").test();
}

#[test]
fn test_test_reflect() {
    refactor("test_reflect").test();
}

#[test]
fn test_uninit_to_default() {
    refactor("uninit_to_default").test();
}
