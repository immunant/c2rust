#![feature(rustc_private)]

mod common;

use common::{command, compile_and_run, refactor, Fixture, COMPILER};
use std::fs;

#[test]
fn reorganize_preserves_unsafe_extern_blocks() {
    let _compiler = COMPILER.lock().unwrap();
    for edition in ["2024", "2021"] {
        let fixture = Fixture::new();
        let main = fixture.write(
            "main.rs",
            r#"
#![feature(register_tool)]
#![register_tool(c2rust)]
mod unit {
    #[c2rust::header_src = "/project/header.h:1"]
    pub mod header_h {
        unsafe extern "C" {
            #[link_name = "abs"]
            pub safe fn safe_abs(value: i32) -> i32;
            pub safe static safe_value: i32;
        }
        unsafe extern "C" {
            #[link_name = "abs"]
            pub unsafe fn unsafe_abs(value: i32) -> i32;
            pub static mut mutable_value: i32;
        }
        unsafe extern "system" {
            pub fn system_function();
        }
    }
}
fn main() {
    println!("{}", unit::header_h::safe_abs(-21)
        + unsafe { unit::header_h::unsafe_abs(-21) });
}
"#,
        );
        assert_eq!(compile_and_run(&main, edition), "42\n");
        refactor(&main, edition, vec![command("reorganize_definitions", &[])]);
        assert_eq!(compile_and_run(&main, edition), "42\n");
        let rewritten = fs::read_to_string(&main).unwrap();
        assert!(!rewritten.contains("header_src"), "{rewritten}");
        assert_eq!(
            rewritten.matches("unsafe extern \"C\"").count(),
            1,
            "{rewritten}"
        );
        assert!(
            rewritten.contains("unsafe extern \"system\""),
            "{rewritten}"
        );
        assert!(
            rewritten.find("unsafe extern \"C\"").unwrap()
                < rewritten.find("unsafe extern \"system\"").unwrap(),
            "{rewritten}"
        );
        assert!(rewritten.contains("pub safe fn safe_abs"), "{rewritten}");
        assert!(
            rewritten.contains("pub unsafe fn unsafe_abs"),
            "{rewritten}"
        );
        assert!(
            rewritten.contains("pub safe static safe_value"),
            "{rewritten}"
        );
        assert!(
            rewritten.contains("pub static mut mutable_value"),
            "{rewritten}"
        );
    }
}

#[test]
fn reorganize_keeps_plain_and_unsafe_extern_blocks_separate() {
    let _compiler = COMPILER.lock().unwrap();
    // Encounter either safety first so regrouping cannot adopt the first block's
    // qualifier for every declaration with the same ABI.
    for (first, second) in [("", "unsafe "), ("unsafe ", "")] {
        let fixture = Fixture::new();
        let main = fixture.write(
            "main.rs",
            &format!(
                r#"
#![feature(register_tool)]
#![register_tool(c2rust)]
mod unit {{
    #[c2rust::header_src = "/project/header.h:1"]
    pub mod header_h {{
        {first}extern "C" {{
            #[link_name = "abs"]
            pub fn first(value: i32) -> i32;
        }}
        {second}extern "C" {{
            #[link_name = "abs"]
            pub fn second(value: i32) -> i32;
        }}
    }}
}}
fn main() {{
    println!("{{}}", unsafe {{ unit::header_h::first(-21) + unit::header_h::second(-21) }});
}}
"#
            ),
        );
        assert_eq!(compile_and_run(&main, "2021"), "42\n");
        refactor(&main, "2021", vec![command("reorganize_definitions", &[])]);
        assert_eq!(compile_and_run(&main, "2021"), "42\n");
        let rewritten = fs::read_to_string(&main).unwrap();
        assert!(!rewritten.contains("header_src"), "{rewritten}");
        assert_eq!(rewritten.matches("extern \"C\"").count(), 2, "{rewritten}");
        assert_eq!(
            rewritten.matches("unsafe extern \"C\"").count(),
            1,
            "{rewritten}"
        );
        assert!(
            rewritten.find("pub fn first").unwrap() < rewritten.find("pub fn second").unwrap(),
            "{rewritten}"
        );
        let unsafe_block = rewritten.split("unsafe extern \"C\"").nth(1).unwrap();
        let unsafe_block = unsafe_block.split('}').next().unwrap();
        let unsafe_fn = if first.is_empty() { "second" } else { "first" };
        assert!(
            unsafe_block.contains(&format!("pub fn {unsafe_fn}")),
            "{rewritten}"
        );
    }
}

#[test]
fn reorganize_deduplicates_only_with_matching_block_safety() {
    let _compiler = COMPILER.lock().unwrap();
    for (first, second) in [("", "unsafe "), ("unsafe ", "")] {
        let fixture = Fixture::new();
        let main = fixture.write(
            "main.rs",
            &format!(
                r#"
#![feature(register_tool)]
#![register_tool(c2rust)]
mod unit {{
    #[c2rust::header_src = "/project/header.h:1"]
    pub mod header_h {{
        {first}extern "C" {{ pub fn abs(value: i32) -> i32; }}
    }}
}}
mod other_unit {{
    #[c2rust::header_src = "/project/header.h:1"]
    pub mod header_h {{
        {second}extern "C" {{ pub fn abs(value: i32) -> i32; }}
    }}
}}
mod duplicate_unit {{
    #[c2rust::header_src = "/project/header.h:1"]
    pub mod header_h {{
        {second}extern "C" {{ pub fn abs(value: i32) -> i32; }}
    }}
}}
fn main() {{
    println!("{{}}", unsafe {{ unit::header_h::abs(-14)
        + other_unit::header_h::abs(-14) + duplicate_unit::header_h::abs(-14) }});
}}
"#
            ),
        );
        assert_eq!(compile_and_run(&main, "2021"), "42\n");
        refactor(&main, "2021", vec![command("reorganize_definitions", &[])]);
        let rewritten = fs::read_to_string(&main).unwrap();
        assert_eq!(rewritten.matches("extern \"C\"").count(), 2, "{rewritten}");
        assert_eq!(
            rewritten.matches("unsafe extern \"C\"").count(),
            1,
            "{rewritten}"
        );
        assert_eq!(rewritten.matches("pub fn abs").count(), 2, "{rewritten}");
        assert_eq!(compile_and_run(&main, "2021"), "42\n");
    }
}
