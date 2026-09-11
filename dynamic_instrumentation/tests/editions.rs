use std::{fs, io::Cursor, path::Path, process::Command};

use c2rust_analysis_rt::{
    events::{Event, EventKind},
    metadata::Metadata,
};

/// Exercise the compiler callback and the instrumented program, including when
/// the runtime is used only by injected MIR and never named by the Rust source.
#[test]
fn instrument_pointer_operations_and_failure_paths_in_all_editions() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let temp = tempfile::tempdir().unwrap();
    let manifest = temp.path().join("Cargo.toml");
    fs::create_dir(temp.path().join("src")).unwrap();
    fs::write(
        temp.path().join("src/main.rs"),
        r#"
fn round_trip(pointer: *mut i32) -> *mut i32 {
    let address = pointer as usize;
    address as *mut i32
}

struct OnDrop;
impl Drop for OnDrop {
    fn drop(&mut self) { eprintln!("cleanup ran"); }
}

fn main() {
    let _cleanup = OnDrop;
    let mut values = [10, 20, 30];
    let slice: &mut [i32] = &mut values;
    assert_eq!(slice.len(), 3);
    let pointer = round_trip(slice.as_mut_ptr());
    unsafe {
        *pointer.add(1) += 7;
        assert_eq!(*pointer.add(1), 27);
    }
    assert_eq!(values, [10, 27, 30]);
    if std::env::var_os("INSTRUMENT_TEST_PANIC").is_some() {
        panic!("intentional unwind");
    }
    println!("pointer operations passed");
}
"#,
    )
    .unwrap();

    for (edition, validation) in [
        ("2015", ""),
        ("2018", " -Zvalidate-mir=no"),
        ("2021", " -Zvalidate-mir=yes"),
        ("2024", " -Zvalidate-mir=no"),
    ] {
        fs::write(
            &manifest,
            format!(
                r#"
[package]
name = "instrument-edition-regression"
version = "0.0.0"
edition = "{edition}"
[dependencies]
c2rust-analysis-rt = {{ path = {:?}, optional = true }}
"#,
                repo.join("analysis/runtime")
            ),
        )
        .unwrap();
        // Match the workspace dependency resolutions, including when the test
        // is run without network access after dependencies have been fetched.
        fs::copy(repo.join("Cargo.lock"), temp.path().join("Cargo.lock")).unwrap();
        let metadata_path = temp.path().join(format!("metadata-{edition}.bc"));
        let events_path = temp.path().join(format!("events-{edition}.bc"));
        let check_metadata_path = temp.path().join(format!("check-metadata-{edition}.bc"));
        let output = Command::new(env!("CARGO_BIN_EXE_c2rust-instrument"))
            .current_dir(temp.path())
            .arg("--metadata")
            .arg(&check_metadata_path)
            .arg(format!("--rustflags=-Zmir-opt-level=0{validation}"))
            .args(["--", "check", "--offline", "--manifest-path"])
            .arg(&manifest)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "edition {edition}:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        let metadata = Metadata::read(&fs::read(check_metadata_path).unwrap()).unwrap();
        assert!(metadata.functions.values().any(|name| name == "round_trip"));
        let output = Command::new(env!("CARGO_BIN_EXE_c2rust-instrument"))
            .current_dir(temp.path())
            .arg("--metadata")
            .arg(&metadata_path)
            // Instrumented MIR must be validated even at optimization level
            // zero and when the caller supplies -Zvalidate-mir=no.
            .arg(format!("--rustflags=-Zmir-opt-level=0{validation}"))
            .args(["--", "run", "--offline", "--manifest-path"])
            .arg(&manifest)
            .env("INSTRUMENT_RUNTIME", "fg")
            .env("INSTRUMENT_BACKEND", "log")
            .env("INSTRUMENT_OUTPUT", &events_path)
            .env("INSTRUMENT_OUTPUT_APPEND", "false")
            .env("METADATA_FILE", &metadata_path)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "edition {edition}:\n{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(String::from_utf8_lossy(&output.stdout).contains("pointer operations passed"));

        let metadata = Metadata::read(&fs::read(&metadata_path).unwrap()).unwrap();
        assert!(metadata.functions.values().any(|name| name == "round_trip"));
        let bytes = fs::read(events_path).unwrap();
        let mut cursor = Cursor::new(bytes.as_slice());
        let mut saw_load = false;
        let mut saw_store = false;
        let mut saw_to_int = false;
        let mut saw_from_int = false;
        while cursor.position() < bytes.len() as u64 {
            // A malformed/truncated event must fail, rather than silently ending
            // the trace and making disappearing coverage look successful.
            let event: Event = bincode::deserialize_from(&mut cursor).unwrap();
            match event.kind {
                EventKind::LoadAddr(_) => saw_load = true,
                EventKind::StoreAddr(_) | EventKind::StoreAddrTaken(_) => saw_store = true,
                EventKind::ToInt(_) => saw_to_int = true,
                EventKind::FromInt(_) => saw_from_int = true,
                _ => {}
            }
            if !matches!(event.kind, EventKind::Done) {
                assert!((event.mir_loc as usize) < metadata.locs.len());
            }
        }
        assert!(saw_load && saw_store && saw_to_int && saw_from_int,
            "edition {edition}: missing expected memory events: load={saw_load}, store={saw_store}, to_int={saw_to_int}, from_int={saw_from_int}");

        let panic_events = temp.path().join(format!("panic-events-{edition}.bc"));
        let output = Command::new(env!("CARGO_BIN_EXE_c2rust-instrument"))
            .current_dir(temp.path())
            .arg("--metadata")
            .arg(&metadata_path)
            .arg(format!("--rustflags=-Zmir-opt-level=0{validation}"))
            .args(["--", "run", "--offline", "--manifest-path"])
            .arg(&manifest)
            .env("INSTRUMENT_RUNTIME", "bg")
            .env("INSTRUMENT_BACKEND", "log")
            .env("INSTRUMENT_OUTPUT", &panic_events)
            .env("INSTRUMENT_OUTPUT_APPEND", "false")
            .env("INSTRUMENT_TEST_PANIC", "true")
            .env("METADATA_FILE", &metadata_path)
            .output()
            .unwrap();
        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("intentional unwind"), "{stderr}");
        assert!(stderr.contains("cleanup ran"), "{stderr}");
        let bytes = fs::read(panic_events).unwrap();
        let mut cursor = Cursor::new(bytes.as_slice());
        let mut saw_done = false;
        while cursor.position() < bytes.len() as u64 {
            let event: Event = bincode::deserialize_from(&mut cursor).unwrap();
            saw_done |= matches!(event.kind, EventKind::Done);
        }
        // The background runtime emits Done only when the injected finalizer
        // is reached, including main's cleanup UnwindResume terminator.
        assert!(
            saw_done,
            "edition {edition}: unwind did not finalize events"
        );
    }

    let metadata_path = temp.path().join("metadata-2024.bc");
    let original_metadata = fs::read(&metadata_path).unwrap();
    fs::write(
        temp.path().join("src/main.rs"),
        "fn main() { let _: i32 = false; }",
    )
    .unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_c2rust-instrument"))
        .current_dir(temp.path())
        .arg("--metadata")
        .arg(&metadata_path)
        .arg("--rustflags=-Zmir-opt-level=0")
        .args(["--", "build", "--offline", "--manifest-path"])
        .arg(&manifest)
        .output()
        .unwrap();
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("mismatched types"), "{stderr}");
    assert_eq!(fs::read(&metadata_path).unwrap(), original_metadata);

    for source in [
        r#"
fn identity(p: *mut i32) -> *mut i32 { p }
fn forward(p: *mut i32) -> *mut i32 { become identity(p); }
fn main() {
    let mut value = 7;
    assert_eq!(forward(&raw mut value), &raw mut value);
}
"#,
        "fn finish() {} fn main() { become finish(); }",
    ] {
        let source_path = temp.path().join("src/main.rs");
        fs::write(
            &source_path,
            format!("#![feature(explicit_tail_calls)]\n#![allow(incomplete_features)]\n{source}"),
        )
        .unwrap();
        // Both pointer-returning tail calls and a tail-calling entrypoint are
        // valid Rust, but must not silently produce incomplete traces.
        let output = Command::new("rustc")
            .arg(&source_path)
            .args(["--edition=2024", "--emit=metadata", "-o"])
            .arg(temp.path().join("tail-call"))
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        for action in ["check", "build"] {
            let output = Command::new(env!("CARGO_BIN_EXE_c2rust-instrument"))
                .current_dir(temp.path())
                .arg("--metadata")
                .arg(&metadata_path)
                .args(["--", action, "--offline", "--manifest-path"])
                .arg(&manifest)
                .output()
                .unwrap();
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(!output.status.success(), "tail call was silently accepted");
            assert!(
                stderr.contains("c2rust-instrument does not support explicit tail calls"),
                "{stderr}"
            );
            assert!(!stderr.contains("internal compiler error"), "{stderr}");
            assert_eq!(fs::read(&metadata_path).unwrap(), original_metadata);
        }
    }
}
