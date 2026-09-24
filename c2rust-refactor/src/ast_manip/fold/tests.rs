use super::*;
use rustc_ast::token::{Delimiter, IdentIsRaw, InvisibleOrigin, MetaVarKind, Token, TokenKind};
use rustc_ast::tokenstream::{DelimSpacing, DelimSpan, Spacing, TokenStream, TokenTree};
use rustc_session::parse::ParseSess;
use rustc_span::{create_default_session_globals_then, BytePos, FileName, Symbol, DUMMY_SP};

fn parse(source: &str) -> Crate {
    let session = ParseSess::new(vec![]);
    rustc_parse::new_parser_from_source_str(
        &session,
        FileName::Custom("visitor-test.rs".into()),
        source.into(),
    )
    .unwrap()
    .parse_crate_mod()
    .unwrap()
}

fn item_kind_order(target: &mut impl MutVisit) -> Vec<String> {
    let mut order = Vec::new();
    <ItemKind as MutVisitNodes>::visit(target, |kind| match kind {
        ItemKind::Fn(f) => order.push(format!("fn:{}", f.body.as_ref().unwrap().stmts.len())),
        ItemKind::Mod(..) => order.push("mod".into()),
        _ => panic!("unexpected item kind"),
    });
    order
}

#[test]
fn item_kind_callbacks_include_standalone_roots_and_nested_items_once() {
    create_default_session_globals_then(|| {
        let mut krate = parse("mod outer { fn parent() { fn child() {} let x = 1; } }");
        let expected = ["fn:0", "fn:2", "mod"];
        assert_eq!(item_kind_order(&mut krate), expected);
        assert_eq!(item_kind_order(&mut krate.items[0].kind), expected);
        let mut wrapped = vec![Some(krate.items[0].kind.clone())];
        assert_eq!(item_kind_order(&mut wrapped), expected);
    });
}

#[test]
fn generic_bound_callbacks_preserve_nested_bounds_and_precise_captures() {
    create_default_session_globals_then(|| {
        let mut krate = parse(
            "trait Outer<T: Nested<Assoc: Bound>>: Super {} \
             fn capture<T>() -> impl Clone + use<T> { loop {} }",
        );
        let mut bounds = Vec::new();
        <GenericBound as MutVisitNodes>::visit(&mut krate, |bound| {
            bounds.push(match bound {
                GenericBound::Trait(poly) => poly
                    .trait_ref
                    .path
                    .segments
                    .last()
                    .unwrap()
                    .ident
                    .name
                    .to_string(),
                GenericBound::Use(..) => "use".into(),
                GenericBound::Outlives(..) => "lifetime".into(),
            });
        });
        assert_eq!(bounds, ["Bound", "Nested", "Super", "Clone", "use"]);
    });
}

#[test]
fn copied_generics_walker_preserves_rustc_callback_order() {
    create_default_session_globals_then(|| {
        let krate = parse("fn f<'a, T: Nested<Assoc: Bound>>() where T: 'a + Clone {}");
        let ItemKind::Fn(f) = &krate.items[0].kind else {
            panic!("expected function")
        };
        let mut upstream_generics = f.generics.clone();
        let mut copied_generics = f.generics.clone();
        #[derive(Default)]
        struct Trace(Vec<String>);
        impl MutVisitor for Trace {
            fn visit_id(&mut self, id: &mut NodeId) {
                self.0.push(format!("id:{id:?}"));
            }
            fn visit_span(&mut self, span: &mut Span) {
                self.0.push(format!("span:{span:?}"));
            }
            fn visit_ident(&mut self, ident: &mut Ident) {
                self.0.push(format!("ident:{}", ident.name));
                walk_ident(self, ident);
            }
        }
        let mut upstream = Trace::default();
        upstream.visit_generics(&mut upstream_generics);
        let mut copied = Trace::default();
        copied_generics.walk(&mut copied);
        assert!(!upstream.0.is_empty());
        assert_eq!(copied.0, upstream.0);
    });
}

#[test]
fn copied_token_walker_preserves_spacing_origin_and_visit_tokens_gate() {
    create_default_session_globals_then(|| {
        let spacing = DelimSpacing::new(Spacing::JointHidden, Spacing::Joint);
        let mut tokens = TokenStream::new(vec![TokenTree::Delimited(
            DelimSpan::dummy(),
            spacing,
            Delimiter::Invisible(InvisibleOrigin::MetaVar(MetaVarKind::Ty)),
            TokenStream::new(vec![TokenTree::Token(
                Token::new(
                    TokenKind::Ident(Symbol::intern("before"), IdentIsRaw::Yes),
                    DUMMY_SP,
                ),
                Spacing::JointHidden,
            )]),
        )]);
        struct Disabled;
        impl MutVisitor for Disabled {
            fn visit_span(&mut self, _: &mut Span) {
                panic!("VISIT_TOKENS is disabled")
            }
            fn visit_ident(&mut self, _: &mut Ident) {
                panic!("VISIT_TOKENS is disabled")
            }
        }
        walk::visit_tts(&mut Disabled, &mut tokens);
        let original = tokens.clone();
        struct Enabled(Vec<&'static str>);
        impl MutVisitor for Enabled {
            const VISIT_TOKENS: bool = true;
            fn visit_span(&mut self, span: &mut Span) {
                self.0.push("span");
                *span = DUMMY_SP.with_hi(BytePos(9)).with_lo(BytePos(7));
            }
            fn visit_ident(&mut self, ident: &mut Ident) {
                self.0.push("ident");
                ident.name = Symbol::intern("after");
                walk_ident(self, ident);
            }
        }
        let mut visitor = Enabled(Vec::new());
        walk::visit_tts(&mut visitor, &mut tokens);
        assert_eq!(visitor.0, ["ident", "span", "span", "span"]);
        let TokenTree::Delimited(span, actual_spacing, delimiter, inner) =
            &tokens.iter().next().unwrap()
        else {
            panic!("expected delimiter")
        };
        assert_eq!(*actual_spacing, spacing);
        assert!(matches!(
            delimiter,
            Delimiter::Invisible(InvisibleOrigin::MetaVar(MetaVarKind::Ty))
        ));
        assert_eq!(span.open.lo(), BytePos(7));
        assert_eq!(span.close.hi(), BytePos(9));
        let TokenTree::Token(token, spacing) = inner.iter().next().unwrap() else {
            panic!("expected token")
        };
        assert_eq!(*spacing, Spacing::JointHidden);
        assert!(
            matches!(token.kind, TokenKind::Ident(name, IdentIsRaw::Yes) if name.as_str() == "after")
        );
        assert_eq!(token.span.lo(), BytePos(7));
        // Rebuilding a shared stream must retain copy-on-write isolation.
        assert!(format!("{original:?}").contains("before"));
    });
}
