#[test]
fn builder_distinguishes_explicit_safe_from_default_foreign_items() {
    use crate::{ast_builder::mk, ast_manip::print::nonterminal_to_string};
    use rustc_ast::{token::Nonterminal, FnRetTy};

    let source = rustc_span::create_default_session_globals_then(|| {
        let decl = mk().fn_decl(
            vec![mk().arg(mk().path_ty("i32"), mk().ident_pat("value"))],
            FnRetTy::Ty(mk().path_ty("i32")),
        );
        let block = mk().unsafe_().extern_("C").foreign_items(vec![
            mk().unsafety("safe").fn_foreign_item("abs", decl.clone()),
            mk().unsafety("normal")
                .fn_foreign_item("default_fn", decl.clone()),
            mk().unsafety("unsafe").fn_foreign_item("unsafe_fn", decl),
            mk().unsafety("safe")
                .static_foreign_item("SAFE_VALUE", mk().path_ty("i32")),
            mk().unsafety("")
                .static_foreign_item("DEFAULT_VALUE", mk().path_ty("i32")),
        ]);
        let printed = nonterminal_to_string(&Nonterminal::NtItem(block));
        assert!(printed.contains("safe fn abs("), "{printed}");
        assert!(printed.contains("unsafe fn unsafe_fn("), "{printed}");
        assert!(!printed.contains("safe fn default_fn("), "{printed}");
        assert!(printed.contains("safe static SAFE_VALUE:"), "{printed}");
        assert!(!printed.contains("safe static DEFAULT_VALUE:"), "{printed}");
        format!("{printed}\nfn main() {{ println!(\"{{}}\", abs(-42)); }}\n")
    });
    let dir = std::env::temp_dir().join(format!("c2rust-builder-safety-{}", std::process::id()));
    std::fs::create_dir(&dir).unwrap();
    let main = dir.join("safe_foreign.rs");
    let executable = dir.join("safe_foreign");
    std::fs::write(&main, source).unwrap();
    for edition in ["2021", "2024"] {
        let output = std::process::Command::new("rustc")
            .arg(&main)
            .args(["--edition", edition, "-o"])
            .arg(&executable)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        let output = std::process::Command::new(&executable).output().unwrap();
        assert!(output.status.success());
        assert_eq!(output.stdout, b"42\n");
    }
    std::fs::remove_dir_all(dir).unwrap();
}
