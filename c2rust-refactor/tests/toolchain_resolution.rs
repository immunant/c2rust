#![feature(rustc_private)]

mod common;

use common::{command, compile_and_run, refactor, Fixture, COMPILER};
use std::fs;

#[test]
fn module_resolution_keeps_both_namespaces() {
    let _compiler = COMPILER.lock().unwrap();
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let main = fixture.write(
            "main.rs",
            r#"
mod content {
    pub mod dual { struct Selected; }
    pub fn dual() -> u32 { 20 }
    pub fn number() -> u32 { 21 }
}
mod imported { pub use crate::content::dual as target; }
fn main() { println!("{}", imported::target() + content::number()); }
"#,
        );
        refactor(
            &main,
            edition,
            vec![
                command("select", &["target", "item(imported::target::Selected);"]),
                command("set_visibility", &["pub"]),
                command("clear_marks", &[]),
                command("rewrite_expr", &["21", "22"]),
            ],
        );
        let rewritten = fs::read_to_string(&main).unwrap();
        assert!(rewritten.contains("pub struct Selected"), "{rewritten}");
        assert_eq!(compile_and_run(&main, edition), "42\n");
    }
}

#[test]
fn external_module_rewrite_preserves_comments() {
    let _compiler = COMPILER.lock().unwrap();
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let main = fixture.write(
            "main.rs",
            "mod content; fn main() { println!(\"{}\", content::number()); }\n",
        );
        let module = fixture.write(
            "content.rs",
            "// Keep the comment with the external module's definition.\npub fn number() -> u32 { 21 }\n",
        );
        refactor(&main, edition, vec![command("rewrite_expr", &["21", "42"])]);
        let rewritten = fs::read_to_string(module).unwrap();
        assert!(rewritten.contains("// Keep the comment"), "{rewritten}");
        assert_eq!(compile_and_run(&main, edition), "42\n");
    }
}

#[test]
fn type_queries_preserve_derived_span_identity_binders_and_pointer_headers() {
    let _compiler = COMPILER.lock().unwrap();
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let main = fixture.write(
            "main.rs",
            r#"
#[derive(Clone, Copy, PartialEq, Eq)]
struct Pair { small: u8, large: u32 }
type Borrow = for<'a> fn(&'a u32) -> &'a u32;
fn borrow(value: &u32) -> &u32 { value }
unsafe extern "C" fn ffi(value: u32) -> u32 { value + 1 }
fn first<const N: usize>(values: &[u32; N]) -> u32 { values[0] as u32 }
fn main() {
    let pair = Pair { small: 2, large: 40 };
    assert!(pair == pair.clone());
    let get: Borrow = borrow;
    let call: unsafe extern "C" fn(u32) -> u32 = ffi;
    let values = [*get(&pair.large); 1 + 1];
    let large = first(&values) as u32;
    let narrowing = large as u8;
    println!("{}", unsafe { call(narrowing as u32) } + pair.small as u32 - 1);
}
"#,
        );
        assert_eq!(compile_and_run(&main, edition), "42\n");
        refactor(&main, edition, vec![command("remove_redundant_casts", &[])]);
        let rewritten = fs::read_to_string(&main).unwrap();
        assert!(!rewritten.contains("values[0] as u32"), "{rewritten}");
        assert!(!rewritten.contains("first(&values) as u32"), "{rewritten}");
        assert!(rewritten.contains("large as u8"), "{rewritten}");
        assert!(
            rewritten.contains("unsafe extern \"C\" fn(u32) -> u32"),
            "{rewritten}"
        );
        assert!(
            rewritten.contains("for<'a> fn(&'a u32) -> &'a u32"),
            "{rewritten}"
        );
        assert_eq!(compile_and_run(&main, edition), "42\n");
    }
}
