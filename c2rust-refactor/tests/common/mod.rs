use c2rust_refactor::{file_io::OutputMode, lib_main, Command, Options, RustcArgSource};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command as Process;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

// Diagnostic tracking and the embedded compiler have process-wide hooks.
pub static COMPILER: Mutex<()> = Mutex::new(());
static NEXT_FIXTURE: AtomicUsize = AtomicUsize::new(0);

pub struct Fixture(pub PathBuf);

impl Fixture {
    pub fn new() -> Self {
        let path = std::env::temp_dir().join(format!(
            "c2rust-driver-{}-{}",
            std::process::id(),
            NEXT_FIXTURE.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&path).unwrap();
        Self(path)
    }

    pub fn write(&self, name: &str, source: &str) -> PathBuf {
        let path = self.0.join(name);
        fs::write(&path, source).unwrap();
        path
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.0).unwrap();
    }
}

pub fn command(name: &str, args: &[&str]) -> Command {
    Command {
        name: name.to_owned(),
        args: args.iter().map(|s| (*s).to_owned()).collect(),
    }
}

pub fn refactor(path: &Path, edition: &str, commands: Vec<Command>) {
    lib_main(Options {
        rewrite_modes: vec![OutputMode::InPlace],
        commands,
        rustc_args: RustcArgSource::CmdLine(vec![
            path.to_str().unwrap().to_owned(),
            "--edition".to_owned(),
            edition.to_owned(),
        ]),
        cursors: vec![],
        marks: vec![],
        plugins: vec![],
        plugin_dirs: vec![],
    })
    .unwrap();
}

pub fn compile_and_run(path: &Path, edition: &str) -> String {
    let executable = path.with_extension("bin");
    // Use the active workspace compiler, which also supplies rustc_private to
    // the refactorer. Do not route this regression through generated-code pins.
    let output = Process::new("rustc")
        .arg(path)
        .args(["--edition", edition, "-o"])
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let output = Process::new(executable).output().unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).unwrap()
}
