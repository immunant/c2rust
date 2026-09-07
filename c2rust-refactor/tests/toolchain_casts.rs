//! Constant folding must use the compilation target's pointer width.
#![feature(rustc_private)]
mod common;
use common::{command, compile_and_run, refactor, Fixture, COMPILER};
use std::fs;

#[test]
fn pointer_sized_literals_above_16_bits_preserve_values_when_simplified() {
    let _compiler = COMPILER.lock().unwrap();
    let source = r#"
fn main() {
    let unsigned = 70000usize as u64;
    let signed = 70000isize as i64;
    let negative = -70000isize as i64;
    let narrowed = 65536usize as u16;
    let minimum = MINIMUMisize as i128;
    println!("{unsigned} {signed} {negative} {narrowed} {minimum}");
}
"#
    .replace("MINIMUM", &isize::MIN.to_string());
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let path = fixture.write("casts.rs", &source);
        let before = compile_and_run(&path, edition);
        assert_eq!(before, format!("70000 70000 -70000 0 {}\n", isize::MIN));
        refactor(&path, edition, vec![command("remove_redundant_casts", &[])]);
        let after = fs::read_to_string(&path).unwrap();
        assert!(after.contains("let unsigned = 70000u64;"), "{after}");
        assert!(after.contains("let signed = 70000i64;"), "{after}");
        assert!(after.contains("let negative = -70000i64;"), "{after}");
        // The narrowing cast changes the value and must remain explicit.
        assert!(after.contains("65536usize as u16"), "{after}");
        assert_eq!(compile_and_run(&path, edition), before);
    }
}
