//! Compiler-migration regressions: rewrite, compile, and execute in both editions.
#![feature(rustc_private)]
mod common;
use common::{command, compile_and_run, refactor, Fixture, COMPILER};
use std::fs;

#[test]
fn transformed_associated_item_macros_preserve_impl_and_trait_contexts() {
    let _compiler = COMPILER.lock().unwrap();
    let source = r#"
macro_rules! methods {
    ($value:expr) => {
        #[cfg_attr(not(any()), inline)]
        fn first() -> i32 { $value }
        fn second() -> i32 { $value * 2 }
    };
}
trait Values { methods!(10 + 20); }
struct Value;
impl Values for Value {}
impl Value { methods!(10 + 20); }
fn main() {
    println!("{} {} {} {}", Value::first(), Value::second(),
             <Value as Values>::first(), <Value as Values>::second());
}
"#;
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let original = fixture.write("associated.rs", source);
        assert_eq!(compile_and_run(&original, edition), "30 60 30 60\n");
        refactor(
            &original,
            edition,
            vec![command("rewrite_expr", &["10 + 20", "3 + 4"])],
        );
        let text = fs::read_to_string(&original).unwrap();
        // The baseline preserves the impl invocation but prints the expanded
        // default trait methods. Keep that established shape in both contexts.
        assert_eq!(text.matches("methods!(3 + 4)").count(), 1, "{text}");
        assert!(text.contains("impl Value { methods!(3 + 4); }"), "{text}");
        assert!(text.contains("fn first() -> i32 { 3 + 4 }"), "{text}");
        assert!(
            text.contains("fn second() -> i32 { (3 + 4) * 2 }"),
            "{text}"
        );
        assert_eq!(compile_and_run(&original, edition), "7 14 7 14\n");
    }
}

#[test]
fn transformed_nested_macro_fragments_preserve_precedence_and_raw_identifiers() {
    let _compiler = COMPILER.lock().unwrap();
    let source = r#"
macro_rules! repeated { ($e:expr) => { $e * 2 + $e }; }
macro_rules! nested { ($e:expr) => { repeated!($e) }; }
macro_rules! call { ($name:ident) => { $name() }; }
macro_rules! tokens { ($($tt:tt)*) => { $($tt)* }; }
macro_rules! multiply { ($e:expr) => { tokens!($e * 2) }; }
fn r#gen() -> i32 { 1 }
fn main() {
    println!("{} {} {}", nested!(10 + 20), call!(r#gen), multiply!(10 + 20));
}
"#;
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let original = fixture.write("input.rs", source);
        assert_eq!(compile_and_run(&original, edition), "90 1 60\n");
        refactor(
            &original,
            edition,
            vec![command("rewrite_expr", &["10 + 20", "3 + 4"])],
        );
        assert_eq!(compile_and_run(&original, edition), "21 1 14\n");
        let text = fs::read_to_string(&original).unwrap();
        assert!(
            text.contains("nested!"),
            "outer invocation was lost: {text}"
        );
        assert!(
            text.contains("call!"),
            "identifier invocation was lost: {text}"
        );
        assert!(
            text.contains("call!(r#gen)"),
            "raw identifier was lost: {text}"
        );
    }
}

#[test]
fn literal_placeholders_round_trip_c_strings_raw_strings_and_non_utf8_bytes() {
    let _compiler = COMPILER.lock().unwrap();
    let source = r##"
fn values() -> (u32, bool, char, u8, &'static str, &'static str, &'static str,
                &'static [u8; 2], &'static std::ffi::CStr, &'static std::ffi::CStr) {
    (123u32, true, '🦀', b'\xFF', "$value", r#"$other"#, "$c2rust_pattern",
     b"\xFF\0", c"\xFF", cr#"raw"#)
}

fn main() { println!("{:?}", values()); }
"##;
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let original = fixture.write("literals.rs", source);
        let before = compile_and_run(&original, edition);
        refactor(
            &original,
            edition,
            vec![
                command(
                    "select",
                    &[
                        "sample",
                        "crate; desc(fn && name(\"values\")); desc(stmt); desc(expr);",
                    ],
                ),
                command(
                    "rewrite_expr",
                    &["$value:Lit", "parse!(dbg!($value))", "sample"],
                ),
            ],
        );
        assert_eq!(compile_and_run(&original, edition), before);
        let text = fs::read_to_string(&original).unwrap();
        assert_eq!(
            text.matches("dbg!").count(),
            10,
            "all literal kinds, including both C strings, must be materialized: {text}"
        );

        // A normal string containing a placeholder's name is an exact literal
        // pattern. It must not capture the other strings or the marker's name.
        refactor(
            &original,
            edition,
            vec![command("rewrite_expr", &["\"$value\"", "\"changed\""])],
        );
        let output = compile_and_run(&original, edition);
        let expected = before.replace("$value", "changed");
        assert_eq!(output, expected);
    }
}

#[test]
fn materialized_fragments_preserve_baseline_token_consumer_behavior() {
    let _compiler = COMPILER.lock().unwrap();
    for edition in ["2021", "2024"] {
        for (input, replacement, printed, before, expected) in [
            (
                "1 + 2",
                "parse!(stringify!($e))",
                "stringify!(1 + 2)",
                "3\n",
                "1 + 2\n",
            ),
            (
                "1 + 2",
                "parse!(stringify!($e * 2))",
                "stringify!(1 + 2 * 2)",
                "3\n",
                "1 + 2 * 2\n",
            ),
            (
                "1 + 2",
                "parse!(double_tt!($e))",
                "double_tt!(1 + 2)",
                "3\n",
                "5\n",
            ),
            (
                "-1i32",
                "parse!(concat!($e))",
                "concat!(-1i32)",
                "-1\n",
                "-1\n",
            ),
            (
                "-1i32",
                "parse!(dbg!($e.abs()))",
                "dbg!(-1i32.abs())",
                "-1\n",
                "-1\n",
            ),
        ] {
            let source = r#"
macro_rules! double_tt { ($($t:tt)*) => { $($t)* * 2 }; }
fn main() { let value = __VALUE__; println!("{}", value); }
"#
            .replace("__VALUE__", input);
            let fixture = Fixture::new();
            let original = fixture.write("tokens.rs", &source);
            assert_eq!(compile_and_run(&original, edition), before);
            refactor(
                &original,
                edition,
                vec![
                    command(
                        "select",
                        &[
                            "sample",
                            "crate; desc(fn && name(\"main\")); desc(stmt); first; child(expr);",
                        ],
                    ),
                    command("rewrite_expr", &["$e:Expr", replacement, "sample"]),
                ],
            );
            let text = fs::read_to_string(&original).unwrap();
            assert!(
                text.contains(printed),
                "baseline fragment syntax changed: {text}"
            );
            // These outputs were independently confirmed using the old compiler
            // and refactor binary. In particular, text serialization does not
            // preserve an opaque expression boundary in arbitrary tt consumers:
            // double_tt!(1 + 2) is 5, and dbg!(-1i32.abs()) is -1.
            assert_eq!(compile_and_run(&original, edition), expected);
        }
    }
}

#[test]
fn converted_format_arguments_keep_cast_values_and_execute() {
    let _compiler = COMPILER.lock().unwrap();
    let source = r#"
#[allow(non_camel_case_types)]
mod libc { pub type c_int = i32; pub type c_uint = u32; }
unsafe extern "C" { fn printf(format: *const std::ffi::c_char, ...) -> i32; }
fn emit(args: std::fmt::Arguments<'_>) -> i32 { print!("{args}"); 0 }
fn main() {
    unsafe {
        printf(b"%d %c %x\n\0".as_ptr() as *const std::ffi::c_char, 7i32, 65i32, 255u32);
    }
}
"#;
    for edition in ["2021", "2024"] {
        let fixture = Fixture::new();
        let original = fixture.write("formats.rs", source);
        assert_eq!(compile_and_run(&original, edition), "7 A ff\n");
        refactor(
            &original,
            edition,
            vec![
                command(
                    "select",
                    &["target", "crate; desc(fn && name(\"printf\"));"],
                ),
                command("mark_arg_uses", &["0", "target"]),
                command("convert_format_args", &[]),
                command("rewrite_expr", &["def!(crate::printf)($fmt)", "emit($fmt)"]),
            ],
        );
        let text = fs::read_to_string(&original).unwrap();
        assert!(text.contains("format_args!"), "conversion was lost: {text}");
        assert!(
            text.contains("as u8 as char"),
            "character cast was lost: {text}"
        );
        assert_eq!(compile_and_run(&original, edition), "7 A ff\n");
    }
}
