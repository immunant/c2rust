mod common;

use c2rust_rust_tools::RustEdition::Edition2021;

use common::config;

fn run_transpiler() {
    let fixture =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/relooper/issue_1821.c");
    let output_dir = c2rust_transpile::TempDir::new().unwrap();
    let (_compile_commands_dir, compile_commands) =
        c2rust_transpile::create_temp_compile_commands(&[fixture]);

    let mut cfg = config(Edition2021);
    cfg.disable_rustfmt = true;
    cfg.emit_c_decl_map = false;
    cfg.output_dir = Some(output_dir.path().to_owned());
    c2rust_transpile::transpile(cfg, &compile_commands, &[]);
}

#[test]
#[cfg_attr(
    debug_assertions,
    ignore = "the relooper exhausts its stack in debug builds"
)]
fn issue_1821_transpiles_quickly() {
    // HACK: Use a thread with a larger stack size. Relooper is tail-recursive,
    // and long functions can cause us to recurse far enough to overflow the
    // stack. We should fix that issue, but for now we work around it in the
    // test by using a thread with a larger stack size.
    //
    // See https://github.com/immunant/c2rust/issues/1908
    std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(run_transpiler)
        .unwrap()
        .join()
        .unwrap();
}
