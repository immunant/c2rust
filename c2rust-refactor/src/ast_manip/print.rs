//! Print transformed macro fragments from their AST, rather than cached tokens.
//!
//! rustc d117b7f211835282b3b177dc64245fff0327c04c changed nonterminal
//! printing to use TokenStream::from_nonterminal_ast. Those tokens still describe
//! the original source after a refactoring changes the AST.
use super::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_ast::token::{Delimiter, InvisibleOrigin, Nonterminal, Token, TokenKind};
use rustc_ast::tokenstream::{DelimSpacing, DelimSpan, Spacing, TokenStream, TokenTree};
use rustc_ast::*;
use rustc_ast_pretty::pprust::{self, PrintState};
use rustc_session::parse::ParseSess;
use rustc_span::FileName;

pub fn is_interpolated(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Interpolated(..) | TokenKind::NtIdent(..) | TokenKind::NtLifetime(..)
    )
}

pub fn token_to_string(token: &Token) -> String {
    match &token.kind {
        TokenKind::Interpolated(nt) => nonterminal_to_string(nt),
        _ => pprust::token_to_string(token).into_owned(),
    }
}

// The compiler's AST printer recursively prints macro and attribute token streams.
// Materialize interpolations in a print-only copy, including nested macro calls.
// The real AST keeps its spans, hygiene, NodeIds, and interpolated fragments.
fn printable_tokens(tokens: &TokenStream) -> TokenStream {
    tokens
        .iter()
        .map(|tree| match tree {
            TokenTree::Token(token, spacing) if is_interpolated(&token.kind) => {
                // ParseSess inherits the root expansion's edition. The span
                // override below keeps the fragment's original span; this
                // temporary SourceMap is used only to lex print-only tokens.
                let session = ParseSess::new(vec![]);
                let source = token_to_string(token);
                let parsed = rustc_parse::unwrap_or_emit_fatal(rustc_parse::source_str_to_stream(
                    &session,
                    FileName::macro_expansion_source_code(&source),
                    source,
                    Some(token.span),
                ));
                // Keep the baseline printer's bare fragment spelling. Adding
                // physical parentheses is observable to stringify! and token
                // matchers. Text cannot encode an opaque AST boundary, so the
                // historical serialization limit remains explicit in tests.
                TokenTree::Delimited(
                    DelimSpan::from_single(token.span),
                    DelimSpacing::new(Spacing::JointHidden, *spacing),
                    Delimiter::Invisible(InvisibleOrigin::FlattenToken),
                    parsed,
                )
            }
            TokenTree::Token(..) => tree.clone(),
            TokenTree::Delimited(span, spacing, delim, inner) => {
                TokenTree::Delimited(*span, *spacing, *delim, printable_tokens(inner))
            }
        })
        .collect()
}

struct Prepare;

impl MutVisitor for Prepare {
    fn visit_mac_call(&mut self, mac: &mut MacCall) {
        mut_visit::walk_mac(self, mac);
        mac.args.tokens = printable_tokens(&mac.args.tokens);
    }

    fn visit_macro_def(&mut self, def: &mut MacroDef) {
        mut_visit::walk_macro_def(self, def);
        def.body.tokens = printable_tokens(&def.body.tokens);
    }

    fn visit_attribute(&mut self, attr: &mut Attribute) {
        mut_visit::walk_attribute(self, attr);
        if let AttrKind::Normal(normal) = &mut attr.kind {
            prepare_attr_args(&mut normal.item.args);
        }
    }
}

fn prepare_attr_args(args: &mut AttrArgs) {
    if let AttrArgs::Delimited(args) = args {
        args.tokens = printable_tokens(&args.tokens);
    }
}

pub fn nonterminal_to_string(nt: &Nonterminal) -> String {
    let mut token = Token::new(
        TokenKind::Interpolated(rustc_data_structures::sync::Lrc::new(nt.clone())),
        rustc_span::DUMMY_SP,
    );
    mut_visit::visit_token(&mut Prepare, &mut token);
    let TokenKind::Interpolated(nt) = &token.kind else {
        unreachable!()
    };
    let original = render_nonterminal(nt);
    // This compiler's print_type calls print_generic_params for UnsafeBinder,
    // which omits empty parameters and produces `unsafe T` instead of
    // `unsafe<> T`. There is no type annotation hook. Adapt only the print copy
    // using names absent from its entire rendered source (including literals),
    // then restore the exact binder text before returning or parsing it.
    let mut binders = EmptyBinders {
        original: &original,
        replacements: Vec::new(),
    };
    mut_visit::visit_token(&mut binders, &mut token);
    if binders.replacements.is_empty() {
        return original;
    }
    let TokenKind::Interpolated(nt) = token.kind else {
        unreachable!()
    };
    let mut rendered = render_nonterminal(&nt);
    // Outer binder text may contain markers for previously visited inner
    // binders, so expand the outer markers first.
    for (marker, replacement) in binders.replacements.into_iter().rev() {
        rendered = rendered.replace(&marker, &replacement);
    }
    rendered
}

struct EmptyBinders<'a> {
    original: &'a str,
    replacements: Vec<(String, String)>,
}

impl MutVisitor for EmptyBinders<'_> {
    fn visit_ty(&mut self, ty: &mut P<Ty>) {
        mut_visit::walk_ty(self, ty);
        let TyKind::UnsafeBinder(binder) = &ty.kind else {
            return;
        };
        if !binder.generic_params.is_empty() {
            return;
        }
        let replacement = format!("unsafe<> {}", pprust::ty_to_string(&binder.inner_ty));
        let mut index = self.replacements.len();
        let marker = loop {
            // The trailing separator prevents marker 1 from matching marker 10.
            let candidate = format!("__c2rust_empty_binder_{index}__");
            if !self.original.contains(&candidate)
                && !self.replacements.iter().any(|(name, _)| *name == candidate)
            {
                break candidate;
            }
            index += 1;
        };
        ty.kind = crate::ast_builder::mk()
            .path_ty(marker.as_str())
            .kind
            .clone();
        self.replacements.push((marker, replacement));
    }
}

fn render_nonterminal(nt: &Nonterminal) -> String {
    let printer = pprust::State::new();
    match nt {
        Nonterminal::NtItem(item) => printer.item_to_string(item),
        Nonterminal::NtBlock(block) => printer.block_to_string(block),
        Nonterminal::NtStmt(stmt) => printer.stmt_to_string(stmt),
        Nonterminal::NtPat(pat) => printer.pat_to_string(pat),
        Nonterminal::NtExpr(expr) | Nonterminal::NtLiteral(expr) => printer.expr_to_string(expr),
        Nonterminal::NtTy(ty) => printer.ty_to_string(ty),
        Nonterminal::NtPath(path) => printer.path_to_string(path),
        Nonterminal::NtVis(vis) => printer.vis_to_string(vis),
        Nonterminal::NtMeta(meta) => {
            let mut meta = meta.clone();
            prepare_attr_args(&mut meta.args);
            printer.attr_item_to_string(&meta)
        }
    }
}

pub fn tokens_to_string(tokens: &TokenStream) -> String {
    pprust::tts_to_string(&printable_tokens(tokens))
}

pub fn attribute_to_string(attr: &Attribute) -> String {
    let mut attr = attr.clone();
    Prepare.visit_attribute(&mut attr);
    pprust::attribute_to_string(&attr)
}

pub fn foreign_item_to_string(item: &ForeignItem) -> String {
    // rustc made print_foreign_item private. Printing inside a fixed, empty
    // extern wrapper uses its exact foreign-item printer (including `safe`
    // statics); its first and last braces delimit the wrapper alone.
    let wrapper = crate::ast_builder::mk().foreign_items(vec![P(item.clone())]);
    let text = nonterminal_to_string(&Nonterminal::NtItem(wrapper));
    text[text.find('{').unwrap() + 1..text.rfind('}').unwrap()]
        .trim()
        .into()
}

pub fn param_to_string(param: &Param) -> String {
    // A fixed function without generics, result type, or body exposes rustc's
    // private parameter printer, including explicit self and parameter attrs.
    let decl = crate::ast_builder::mk()
        .fn_decl(vec![param.clone()], FnRetTy::Default(rustc_span::DUMMY_SP));
    let wrapper = crate::ast_builder::mk().fn_item("__c2rust_print_param", decl, None::<P<Block>>);
    let text = nonterminal_to_string(&Nonterminal::NtItem(wrapper));
    text[text.find('(').unwrap() + 1..text.rfind(')').unwrap()]
        .trim()
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rustc_ast::token::{IdentIsRaw, Lit, LitKind};
    use rustc_data_structures::sync::Lrc;
    use rustc_parse::parser::ForceCollect;
    use rustc_span::{create_default_session_globals_then, Ident, Symbol, DUMMY_SP};

    fn expression(session: &ParseSess, source: &str) -> P<Expr> {
        rustc_parse::new_parser_from_source_str(
            session,
            FileName::macro_expansion_source_code(source),
            source.into(),
        )
        .unwrap()
        .parse_expr_force_collect()
        .unwrap()
    }

    fn changed_expression(session: &ParseSess) -> P<Expr> {
        let mut expr = expression(session, "1 + 2");
        assert!(expr.tokens.is_some());
        let ExprKind::Binary(_, _, rhs) = &mut expr.kind else {
            panic!("expected binary")
        };
        rhs.kind = ExprKind::Lit(Lit::new(LitKind::Integer, Symbol::intern("9"), None));
        expr
    }

    #[test]
    fn transformed_nonterminal_ignores_stale_captured_tokens() {
        create_default_session_globals_then(|| {
            let session = ParseSess::new(vec![]);
            let mut expr = changed_expression(&session);
            expr.id = NodeId::from_u32(123);
            let span = expr.span;
            let token = Token::new(
                TokenKind::Interpolated(Lrc::new(Nonterminal::NtExpr(expr))),
                span,
            );
            assert_eq!(pprust::token_to_string(&token), "1 + 2");
            assert_eq!(token_to_string(&token), "1 + 9");
            let TokenKind::Interpolated(nt) = &token.kind else {
                unreachable!()
            };
            let Nonterminal::NtExpr(expr) = &**nt else {
                unreachable!()
            };
            assert_eq!(expr.id, NodeId::from_u32(123));
            assert_eq!(expr.span, span);
            assert_eq!(pprust::token_to_string(&token), "1 + 2");
        });
    }

    #[test]
    fn transformed_nonterminal_is_printed_inside_nested_macro_tokens() {
        create_default_session_globals_then(|| {
            let session = ParseSess::new(vec![]);
            let changed = changed_expression(&session);
            let mut outer = expression(&session, "nested!(0)");
            let ExprKind::MacCall(mac) = &mut outer.kind else {
                panic!("expected macro")
            };
            mac.args.tokens = TokenStream::token_alone(
                TokenKind::Interpolated(Lrc::new(Nonterminal::NtExpr(changed))),
                DUMMY_SP,
            );
            assert_eq!(
                nonterminal_to_string(&Nonterminal::NtExpr(outer)),
                "nested!(1 + 9)"
            );
        });
    }

    #[test]
    fn materialized_expression_preserves_baseline_spelling_before_an_operator() {
        create_default_session_globals_then(|| {
            let session = ParseSess::new(vec![]);
            let expr = changed_expression(&session);
            let tokens = TokenStream::new(vec![
                TokenTree::Token(
                    Token::new(
                        TokenKind::Interpolated(Lrc::new(Nonterminal::NtExpr(expr))),
                        DUMMY_SP,
                    ),
                    Spacing::Alone,
                ),
                TokenTree::token_alone(
                    TokenKind::BinOp(rustc_ast::token::BinOpToken::Star),
                    DUMMY_SP,
                ),
                TokenTree::token_alone(
                    TokenKind::Literal(Lit::new(LitKind::Integer, Symbol::intern("2"), None)),
                    DUMMY_SP,
                ),
            ]);
            let text = tokens_to_string(&tokens);
            // The old AST printer serialized this fragment without physical
            // grouping. Re-parsing its text loses the invisible AST boundary;
            // preserve that established behavior instead of changing observable
            // stringify!/tt output while migrating the compiler API.
            assert_eq!(text, "1 + 9 * 2");
            let reparsed = expression(&session, &text);
            let ExprKind::Binary(op, _, rhs) = &reparsed.kind else {
                panic!("expected baseline addition: {text}")
            };
            assert_eq!(op.node, BinOpKind::Add);
            assert!(matches!(&rhs.kind, ExprKind::Binary(op, ..) if op.node == BinOpKind::Mul));
        });
    }

    #[test]
    fn foreign_item_and_parameter_printers_preserve_contextual_syntax() {
        create_default_session_globals_then(|| {
            let session = ParseSess::new(vec![]);
            let mut parser = rustc_parse::new_parser_from_source_str(&session, FileName::Custom("foreign.rs".into()),
                "unsafe extern \"C\" { pub safe static VALUE: u32; pub unsafe fn r#gen(#[allow(unused)] arg: extern \"C\" fn(u8, u16)); }".into()).unwrap();
            let item = parser.parse_item(ForceCollect::Yes).unwrap().unwrap();
            let ItemKind::ForeignMod(module) = &item.kind else {
                panic!("expected foreign module")
            };
            assert_eq!(
                foreign_item_to_string(&module.items[0]),
                "pub safe static VALUE: u32;"
            );
            let rendered = foreign_item_to_string(&module.items[1]);
            assert!(rendered.starts_with("pub unsafe fn"));
            let ForeignItemKind::Fn(f) = &module.items[1].kind else {
                panic!("expected function")
            };
            assert_eq!(
                param_to_string(&f.sig.decl.inputs[0]),
                "#[allow(unused)] arg: extern \"C\" fn(u8, u16)"
            );
        });
    }

    #[test]
    fn moved_identifier_and_lifetime_nonterminals_keep_raw_syntax() {
        create_default_session_globals_then(|| {
            for (name, lifetime, expected) in [("gen", false, "r#gen"), ("'gen", true, "'r#gen")] {
                let ident = Ident::new(Symbol::intern(name), DUMMY_SP);
                let kind = if lifetime {
                    TokenKind::NtLifetime(ident, IdentIsRaw::Yes)
                } else {
                    TokenKind::NtIdent(ident, IdentIsRaw::Yes)
                };
                assert!(is_interpolated(&kind));
                assert_eq!(token_to_string(&Token::new(kind, DUMMY_SP)), expected);
            }
        });
    }

    #[test]
    fn empty_unsafe_binders_print_in_nested_types_without_marker_collisions() {
        create_default_session_globals_then(|| {
            let session = ParseSess::new(vec![]);
            let source = r#"type __c2rust_empty_binder_0__ =
                (unsafe<> unsafe fn(unsafe<> ()) -> unsafe<> (),
                 [unsafe<> (); { let _ = "__c2rust_empty_binder_1__"; 1 }]);"#;
            let item = rustc_parse::new_parser_from_source_str(
                &session,
                FileName::macro_expansion_source_code(source),
                source.into(),
            )
            .unwrap()
            .parse_item(ForceCollect::Yes)
            .unwrap()
            .unwrap();
            let nt = Nonterminal::NtItem(item);
            let original = pprust::State::new().item_to_string(match &nt {
                Nonterminal::NtItem(item) => item,
                _ => unreachable!(),
            });
            assert!(!original.contains("unsafe<>"));
            let rendered = nonterminal_to_string(&nt);
            assert_eq!(rendered.matches("unsafe<>").count(), 4, "{rendered}");
            assert!(rendered.contains("type __c2rust_empty_binder_0__"));
            assert!(rendered.contains("\"__c2rust_empty_binder_1__\""));
            assert!(rendered.contains("unsafe<> unsafe fn(unsafe<> ()) -> unsafe<> ()"));
            let reparsed = rustc_parse::new_parser_from_source_str(
                &session,
                FileName::macro_expansion_source_code(&rendered),
                rendered.clone(),
            )
            .unwrap()
            .parse_item(ForceCollect::Yes)
            .unwrap()
            .unwrap();
            assert_eq!(
                nonterminal_to_string(&Nonterminal::NtItem(reparsed)),
                rendered
            );
            // The live fragment and its captured token cache were not changed.
            assert_eq!(
                pprust::State::new().item_to_string(match &nt {
                    Nonterminal::NtItem(item) => item,
                    _ => unreachable!(),
                }),
                original
            );
        });
    }
}
