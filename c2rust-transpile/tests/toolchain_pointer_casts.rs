//! Generated pointer casts must work on both generated-code and workspace pins.
mod common;

use c2rust_rust_tools::RustEdition;
use std::fs;
use std::path::Path;
use std::process::{Command, Output};
use tempfile::TempDir;

fn successful(command: &mut Command) -> Output {
    let output = command.output().unwrap();
    assert!(
        output.status.success(),
        "{command:?}: {}\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    output
}

fn compile_and_run(source: &Path, edition: RustEdition, toolchain: Option<&str>) -> Vec<u8> {
    let executable = source.with_extension(format!("{}.bin", toolchain.unwrap_or("workspace")));
    successful(
        Command::new("rustc")
            .args(toolchain)
            .args([
                "--edition",
                edition.as_str(),
                "--crate-name",
                "pointer_casts",
            ])
            .arg(source)
            .arg("-o")
            .arg(&executable),
    );
    successful(&mut Command::new(executable)).stdout
}

#[test]
fn integer_pointer_casts_preserve_width_mutability_and_single_evaluation() {
    let fixture = TempDir::new().unwrap();
    let source = fixture.path().join("pointer_casts.c");
    fs::write(
        &source,
        r#"
typedef __UINTPTR_TYPE__ uintptr_t;
extern int printf(const char *, ...);
static int values[3] = {11, 22, 33};
static int calls;
static const int *constant_pointer = (const int *)16;
enum small { ZERO = 0, ONE = 1 };
static int *next_pointer(void) { calls++; return &values[1]; }
static uintptr_t next_address(uintptr_t address) { calls++; return address; }
int main(void) {
    uintptr_t address = (uintptr_t)next_pointer();
    if (calls != 1 || address != (uintptr_t)&values[1]) return 1;
    int *mutable = (int *)next_address(address);
    if (calls != 2 || mutable != &values[1]) return 2;
    *mutable = 47;
    const int *immutable = (const int *)next_address(address);
    if (calls != 3 || *immutable != 47 || values[0] != 11 || values[2] != 33) return 3;
    unsigned long long wider = (unsigned long long)immutable;
    if ((uintptr_t)wider != address) return 4;
    unsigned char narrow = (unsigned char)immutable;
    if (narrow != (unsigned char)address) return 5;
    void *negative = (void *)(long)-1;
    if ((uintptr_t)negative != ~(uintptr_t)0) return 6;
    void *from_enum = (void *)ONE;
    if ((uintptr_t)from_enum != 1 || (enum small)(void *)0 != ZERO) return 7;
    __uint128_t wide = ((__uint128_t)1 << (sizeof(void *) * 8)) | address;
    int *from_wide = (int *)wide;
    if (from_wide != mutable || *from_wide != 47) return 8;
    uintptr_t zero = 0;
    if ((const int *)zero != 0 || (uintptr_t)constant_pointer != 16) return 9;
    printf("%d %d %d %d\n", values[0], *immutable, values[2], calls);
    return 0;
}
"#,
    )
    .unwrap();
    // Each check in the fixture exits unsuccessfully if pointer behavior differs.
    let expected = b"11 47 33 3\n";

    let (_commands_dir, commands) = c2rust_transpile::create_temp_compile_commands(&[source]);
    for edition in RustEdition::ALL.iter().copied() {
        let output_dir = fixture.path().join(edition.as_str());
        let mut config = common::config(edition);
        config.emit_c_decl_map = false;
        config.output_dir = Some(output_dir.clone());
        c2rust_transpile::transpile(config, &commands, &[]);
        let generated = output_dir.join("src/pointer_casts.rs");
        assert_eq!(
            compile_and_run(&generated, edition, Some(edition.toolchain())),
            expected
        );
        if edition == RustEdition::Edition2021 {
            assert_eq!(compile_and_run(&generated, edition, None), expected);
        }
    }
}
