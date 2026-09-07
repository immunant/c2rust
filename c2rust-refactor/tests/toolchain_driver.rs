#![feature(rustc_private)]

mod common;

use common::{command, compile_and_run, refactor, Fixture, COMPILER};
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command as Process, Stdio};
use std::sync::mpsc;
use std::time::Duration;

#[test]
fn cargo_derived_rustc_arguments_preserve_the_input_and_sysroot() {
    let fixture = Fixture::new();
    fixture.write(
        "Cargo.toml",
        "[package]\nname=\"driver_fixture\"\nversion=\"0.0.0\"\nedition=\"2021\"\n[[bin]]\nname=\"driver_fixture\"\npath=\"main.rs\"\n",
    );
    let main = fixture.write("main.rs", "fn main() { println!(\"{}\", 1i32); }\n");
    let output = Process::new(env!("CARGO_BIN_EXE_c2rust-refactor"))
        .current_dir(&fixture.0)
        .args([
            "--cargo",
            "--bin",
            "driver_fixture",
            "-r",
            "inplace",
            "rewrite_expr",
            "1i32",
            "42i32",
        ])
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(fs::read_to_string(&main).unwrap().contains("42i32"));
    assert_eq!(compile_and_run(&main, "2021"), "42\n");
}

#[test]
fn rust_2024_prelude_is_used_during_typed_refactoring() {
    let _compiler = COMPILER.lock().unwrap();
    let fixture = Fixture::new();
    let main = fixture.write(
        "main.rs",
        "fn main() { let _future = std::future::ready(()).into_future(); let value = unsafe { 42 }; println!(\"{}\", value); }\n",
    );
    // IntoFuture entered the prelude in edition 2024. A driver that silently
    // selects edition 2021 fails typechecking and skips removing this unsafe.
    refactor(&main, "2024", vec![command("fix_unused_unsafe", &[])]);
    let rewritten = fs::read_to_string(&main).unwrap();
    assert!(!rewritten.contains("unsafe"), "{rewritten}");
    assert_eq!(compile_and_run(&main, "2024"), "42\n");
}

#[test]
fn commit_reloads_rewritten_external_source_in_both_editions() {
    let _compiler = COMPILER.lock().unwrap();
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let main = fixture.write(
            "main.rs",
            "mod value; fn main() { println!(\"{}\", value::get()); }\n",
        );
        let module = fixture.write(
            "value.rs",
            "pub fn get() -> i32 { let value = 1i32; value }\n",
        );
        refactor(
            &main,
            edition,
            vec![
                command("rewrite_expr", &["1i32", "200i32"]),
                command("commit", &[]),
                command("rewrite_expr", &["200i32", "333i32"]),
            ],
        );
        let rewritten = fs::read_to_string(module).unwrap();
        assert!(rewritten.contains("333i32"), "{rewritten}");
        assert_eq!(compile_and_run(&main, edition), "333\n");
    }
}

#[test]
fn unused_unsafe_capture_respects_lint_suppression_and_block_spans() {
    let _compiler = COMPILER.lock().unwrap();
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let main = fixture.write(
            "main.rs",
            r#"
fn redundant() -> i32 { let value = unsafe { 10 }; value }
#[allow(unused_unsafe)]
fn suppressed() -> i32 { unsafe { 20 } }
fn required(p: *const i32) -> i32 { unsafe { *p } }
fn main() { let value = 12; println!("{}", redundant() + suppressed() + required(&value)); }
"#,
        );
        refactor(&main, edition, vec![command("fix_unused_unsafe", &[])]);
        let rewritten = fs::read_to_string(&main).unwrap();
        let redundant = rewritten.split("#[allow").next().unwrap();
        assert!(!redundant.contains("unsafe"), "{rewritten}");
        assert!(rewritten.contains("unsafe { 20 }"), "{rewritten}");
        assert!(rewritten.contains("unsafe { *p }"), "{rewritten}");
        assert_eq!(compile_and_run(&main, edition), "42\n");
    }
}

#[test]
fn autoretype_iterations_compose_with_unused_unsafe_diagnostics() {
    let _compiler = COMPILER.lock().unwrap();
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let main = fixture.write(
            "main.rs",
            r#"
fn add(value: i32) -> i32 {
    // Autoretype must restore this annotation after E0282 and typecheck again.
    let parsed: i32 = "1".parse().unwrap();
    let _ = parsed;
    let output: i32 = unsafe { value + 1 };
    output
}

fn main() { println!("{}", add(41)); }
"#,
        );
        refactor(
            &main,
            edition,
            vec![
                command(
                    "select",
                    &[
                        "target",
                        "crate; desc(arg || fn); child(ty && match_ty(i32));",
                    ],
                ),
                command("autoretype", &["target: u32"]),
                command("fix_unused_unsafe", &[]),
            ],
        );
        let rewritten = fs::read_to_string(&main).unwrap();
        assert!(rewritten.contains("value: u32"), "{rewritten}");
        assert!(rewritten.contains("parsed: i32"), "{rewritten}");
        assert!(rewritten.contains("output: u32"), "{rewritten}");
        assert!(!rewritten.contains("unsafe"), "{rewritten}");
        assert_eq!(compile_and_run(&main, edition), "42\n");
    }
}

#[test]
fn accepted_preexisting_errors_do_not_abort_compiler_callback_exit() {
    let _compiler = COMPILER.lock().unwrap();
    let fixture = Fixture::new();
    let main = fixture.write(
        "main.rs",
        "fn broken() { missing_function(); }\nfn main() {}\n",
    );
    refactor(&main, "2024", vec![command("autoretype", &[])]);
    // Acceptance is specific to refactoring preexisting errors: the output is
    // deliberately still ill-typed and must not be represented as compilable.
    let output = Process::new("rustc")
        .arg(&main)
        .args(["--edition", "2024", "--emit=metadata", "-o"])
        .arg(fixture.0.join("out.rmeta"))
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("missing_function"));
}

#[test]
fn parser_failure_and_rejected_new_type_errors_still_fail() {
    let fixture = Fixture::new();
    let main = fixture.write("main.rs", "fn main( {\n");
    let output = Process::new(env!("CARGO_BIN_EXE_c2rust-refactor"))
        .args(["autoretype", "--"])
        .arg(&main)
        .args(["--edition", "2024"])
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("unclosed delimiter"));

    fixture.write(
        "main.rs",
        "fn broken(x: i32) -> i32 { missing_function(); x + 1 }\nfn main() {}\n",
    );
    let output = Process::new(env!("CARGO_BIN_EXE_c2rust-refactor"))
        .args([
            "select",
            "target",
            "crate; desc(arg); child(ty && match_ty(i32));",
            ";",
            "autoretype",
            "target: String",
            "--",
        ])
        .arg(&main)
        .args(["--edition", "2024"])
        .output()
        .unwrap();
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Could not retype crate!"), "{stderr}");
    assert!(stderr.contains("Typechecking failed"), "{stderr}");
}

struct InteractiveChild(Child);

impl Drop for InteractiveChild {
    fn drop(&mut self) {
        // Interactive mode is a long-lived editor service with no quit message.
        let _ = self.0.kill();
        self.0.wait().unwrap();
    }
}

#[test]
fn interactive_commands_reload_current_editor_buffer() {
    let fixture = Fixture::new();
    // The plain test protocol accepts one token as the buffer content. This
    // spelling remains valid Rust and has no spaces, including after rewriting.
    let mut buffer = "fn/**/main(){println!(\"{}\",1i32);}".to_owned();
    let main = fixture.write("main.rs", &buffer);
    let mut child = InteractiveChild(
        Process::new(env!("CARGO_BIN_EXE_c2rust-refactor"))
            .args(["interact", "--"])
            .arg(&main)
            .args(["--edition", "2024"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap(),
    );
    let mut input = child.0.stdin.take().unwrap();
    let output = child.0.stdout.take().unwrap();
    let (send, recv) = mpsc::channel();
    std::thread::spawn(move || {
        for line in BufReader::new(output).lines() {
            if send.send(line.unwrap()).is_err() {
                break;
            }
        }
    });
    writeln!(input, "set-buffers-available {}", main.display()).unwrap();
    for (old, new) in [("1i32", "200i32"), ("200i32", "333i32")] {
        writeln!(input, "run-command rewrite_expr {old} {new}").unwrap();
        input.flush().unwrap();
        let mut requested_buffer = false;
        loop {
            let line = recv.recv_timeout(Duration::from_secs(30)).unwrap();
            if line.starts_with("get-buffer-text ") {
                requested_buffer = true;
                writeln!(input, "buffer-text {} {buffer}", main.display()).unwrap();
                input.flush().unwrap();
            } else if line.starts_with("new-buffer-text ") {
                let mut lines = Vec::new();
                loop {
                    let line = recv.recv_timeout(Duration::from_secs(30)).unwrap();
                    if line == "." {
                        break;
                    }
                    lines.push(line);
                }
                buffer = lines.join("\n");
                assert!(requested_buffer, "command reused cached editor contents");
                assert!(buffer.contains(new), "{buffer}");
                break;
            } else {
                panic!("unexpected editor response: {line}");
            }
        }
    }
    fixture.write("main.rs", &buffer);
    assert_eq!(compile_and_run(&main, "2024"), "333\n");
}
