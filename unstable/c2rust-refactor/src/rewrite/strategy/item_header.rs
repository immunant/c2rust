//! Rewriting strategy for handling changes in `Item` headers.
//!
//! In items such as `pub unsafe fn foo() { ... }`, the representations of the `pub` and `unsafe`
//! qualifiers in the AST don't include any source information.  So if the AST changes to simply
//! `extern "C" fn foo() { ... }`, there is no indication of where to delete the `pub` and `unsafe`
//! or where to insert the `extern "C"` qualifier.
//!
//! This rewrite strategy does its own parsing of the `Item`'s tokens to find source locations for
//! any qualifiers that are present, and also finds valid locations to insert any qualifiers that
//! are absent from the original `Item`.
//!
//! Aside from the special handling of qualifiers, this strategy works the same as `recursive`.
use log::info;
use rustc_ast::token::{Delimiter, Token, TokenKind};
use rustc_ast::tokenstream::{TokenStream, TokenTree};
use rustc_ast::*;
use rustc_span::source_map::{BytePos, Span};
use rustc_span::DUMMY_SP;

use crate::ast_manip::AstEquiv;
use crate::expect;
use crate::rewrite::base::{describe, rewrite_seq_comma_sep};
use crate::rewrite::strategy::print::PrintParse;
use crate::rewrite::{Rewrite, RewriteCtxtRef, TextRewrite};

// struct FnHeaderSpans {
//     vis: Span,
//     constness: Span,
//     unsafety: Span,
//     abi: Span,
//     ident: Span,
// }

// fn start_point(sp: Span) -> Span {
//     sp.with_hi(sp.lo())
// }

fn span_empty(sp: Span) -> bool {
    sp.lo() == sp.hi()
}

// fn find_fn_header_spans<'a>(p: &mut Parser<'a>) -> PResult<'a, FnHeaderSpans> {
//     // Skip over any attributes that were included in the token stream.
//     loop {
//         if crate::matches!([p.token.kind] TokenKind::DocComment(..)) {
//             p.bump();
//         } else if crate::matches!([p.token.kind] TokenKind::Pound) {
//             // I don't think we should ever see inner attributes inside `item.tokens`, but allow
//             // them just in case.
//             p.parse_attribute(true)?;
//         } else {
//             break;
//         }
//     }

//     let spanned_vis = p.parse_visibility(FollowedByType::No)?;
//     let vis = if !spanned_vis.kind.ast_equiv(&VisibilityKind::Inherited) {
//         spanned_vis.span
//     } else {
//         // `Inherited` visibility is implicit - there are no actual tokens.  Insert visibility just
//         // before the next token.
//         start_point(p.token.span)
//     };

//     let constness = if p.eat(&TokenKind::Ident(kw::Const, false)) {
//         p.prev_span
//     } else {
//         start_point(p.token.span)
//     };

//     let unsafety = if p.eat(&TokenKind::Ident(kw::Unsafe, false)) {
//         p.prev_span
//     } else {
//         start_point(p.token.span)
//     };

//     let abi = if p.eat(&TokenKind::Ident(kw::Extern, false)) {
//         let extern_span = p.prev_span;
//         if crate::matches!([p.token.kind] TokenKind::Literal(..)) {
//             // Just assume it's a valid abi string token.  If it wasn't, these tokens wouldn't have
//             // parsed as an item to begin with.
//             p.bump();
//             extern_span.to(p.prev_span)
//         } else {
//             // Implicitly `extern "C"`.
//             extern_span
//         }
//     } else {
//         start_point(p.token.span)
//     };

//     p.expect(&TokenKind::Ident(kw::Fn, false))?;

//     p.parse_ident()?;
//     let ident = p.prev_span;

//     Ok(FnHeaderSpans {
//         vis,
//         constness,
//         unsafety,
//         abi,
//         ident,
//     })
// }

// struct ItemHeaderSpans {
//     vis: Span,
//     ident: Span,
// }

/// Generic parsing function for item headers of the form "<vis> <struct/enum/etc> <ident>".
// fn find_item_header_spans<'a>(p: &mut Parser<'a>) -> PResult<'a, ItemHeaderSpans> {
//     // Skip over any attributes that were included in the token stream.
//     loop {
//         if crate::matches!([p.token.kind] TokenKind::DocComment(..)) {
//             p.bump();
//         } else if crate::matches!([p.token.kind] TokenKind::Pound) {
//             // I don't think we should ever see inner attributes inside `item.tokens`, but allow
//             // them just in case.
//             p.parse_attribute(true)?;
//         } else {
//             break;
//         }
//     }

//     let spanned_vis = p.parse_visibility(false)?;
//     let vis = if !spanned_vis.kind.ast_equiv(&VisibilityKind::Inherited) {
//         spanned_vis.span
//     } else {
//         // `Inherited` visibility is implicit - there are no actual tokens.  Insert visibility just
//         // before the next token.
//         start_point(p.token.span)
//     };

//     let kws = &[
//         kw::Static,
//         kw::Const,
//         kw::Fn,
//         kw::Mod,
//         kw::Type,
//         kw::Enum,
//         kw::Struct,
//         kw::Union,
//         kw::Trait,
//     ];

//     for (i, &kw) in kws.iter().enumerate() {
//         if i < kws.len() - 1 {
//             if p.eat_keyword(kw) {
//                 break;
//             }
//         } else {
//             // Use `expect` for the last one so we produce a parse error on "none of the above".
//             p.expect(&TokenKind::Ident(kw, false))?;
//             break;
//         }
//     }

//     p.parse_ident()?;
//     let ident = p.prev_span;

//     Ok(ItemHeaderSpans { vis, ident })
// }

fn find_fn_header_arg_list(ts: TokenStream, generics_span: Span) -> Option<(TokenStream, Span)> {
    // Take the body of the first paren-delimited subtree that's strictly after `generics_span`.
    ts.into_trees()
        .filter_map(|tt| match tt {
            TokenTree::Delimited(sp, delim, tts) => {
                if delim == Delimiter::Parenthesis && sp.open.lo() >= generics_span.hi() {
                    Some((tts, sp.open.between(sp.close)))
                } else {
                    None
                }
            }
            _ => None,
        })
        .next()
}

/// Record a rewrite of a qualifier, such as `unsafe`.  We make two assumptions:
///  1. If `old_span` is empty, then it is placed at the start of the next token after the place
///     the new qualifier should go.
///  2. If `new_span` is non-empty, then it is followed by a space.
fn record_qualifier_rewrite(old_span: Span, new_span: Span, mut rcx: RewriteCtxtRef) {
    let src_span = if span_empty(old_span) && !span_empty(new_span) {
        // We are inserting some text where there was none before.  We need to extend
        // the source span by one, picking up the trailing space, so that there will be
        // a space between the inserted text and the following token.
        new_span.with_hi(new_span.hi() + BytePos(1))
    } else {
        new_span
    };

    if span_empty(old_span) {
        info!("INSERT (QUAL) {}", describe(rcx.session(), old_span));
        info!("    AT (QUAL) {}", describe(rcx.session(), src_span));
    } else if span_empty(new_span) {
        info!("DELETE (QUAL) {}", describe(rcx.session(), old_span));
    } else {
        info!("REWRITE (QUAL) {}", describe(rcx.session(), old_span));
        info!("   INTO (QUAL) {}", describe(rcx.session(), src_span));
    }

    rcx.record(TextRewrite::new(old_span, src_span));
}

fn rewrite_arg_list_with_tokens(
    old: &[Param],
    new: &[Param],
    args_tokens: TokenStream,
    args_span: Span,
    rcx: RewriteCtxtRef,
) -> bool {
    // Find commas that separate arguments.  This is harder than it should be because type
    // argument lists (`<...>`) aren't `Delimited` like parens etc.  So instead of just looking for
    // commas at top level, we have to find commas that aren't included in any argument span.
    let mut comma_spans = Vec::with_capacity(old.len());
    let mut tt_iter = args_tokens.into_trees();
    for (i, old_arg) in old.iter().enumerate() {
        // First, run `tt_iter` forward until we're past `old_arg`.
        assert!(old_arg.ty.span.hi() > old_arg.pat.span.hi()); // sanity check
        let end_pos = old_arg.ty.span.hi();

        let is_last = i == old.len() - 1;
        let mut past_arg = false;
        loop {
            match tt_iter.next() {
                Some(tt) => {
                    if !past_arg && tt.span().lo() >= end_pos {
                        // This token is just past the end of the current arg.
                        past_arg = true;
                    }
                    if past_arg
                        && crate::matches!([tt] TokenTree::Token(Token {
                        kind: TokenKind::Comma,
                        ..
                    }, _))
                    {
                        // Found the comma following the current arg.
                        comma_spans.push(tt.span());
                        break;
                    }
                }

                None => {
                    if is_last {
                        // It's okay if the trailing comma is missing.  Note we don't check for
                        // `past_arg` because it's possible that the arg includes the last token of
                        // the stream.
                        break;
                    } else {
                        panic!("not enough commas in arg list token stream");
                    }
                }
            }
        }
    }
    assert!(comma_spans.len() == old.len() || comma_spans.len() + 1 == old.len());

    let mut spans_with_commas = Vec::with_capacity(old.len());
    for (i, arg) in old.iter().enumerate() {
        let arg_span = arg.pat.span.to(arg.ty.span);
        // If we want to do something clever to attach comments to the right args, this is the
        // place to do it.
        if let Some(&comma_span) = comma_spans.get(i) {
            spans_with_commas.push(arg_span.to(comma_span));
        } else {
            spans_with_commas.push(arg_span);
        }
    }

    let has_trailing_comma = comma_spans.len() == old.len();
    rewrite_seq_comma_sep(
        old,
        new,
        &spans_with_commas,
        args_span,
        has_trailing_comma,
        rcx,
    )
}

pub fn rewrite(old: &Item, new: &Item, mut rcx: RewriteCtxtRef) -> bool {
    let &Item {
        ident: ref ident1,
        attrs: ref attrs1,
        id: ref id1,
        kind: ref kind1,
        vis: ref vis1,
        span: ref span1,
        tokens: ref tokens1,
    } = old;
    let &Item {
        ident: ref ident2,
        attrs: ref attrs2,
        id: ref id2,
        kind: ref kind2,
        vis: ref vis2,
        span: ref span2,
        tokens: ref _tokens2,
    } = new;

    // We can't do anything without tokens to parse.  (This is not quite true - we could
    // pretty-print and reparse `old`.  But that's a pain, so just require tokens instead.)
    if tokens1.is_none() {
        return false;
    }

    match (kind1, kind2) {
        (
            &ItemKind::Fn(box Fn {
                sig: ref sig1,
                generics: ref generics1,
                body: ref block1,
                ..
            }),
            &ItemKind::Fn(box Fn {
                sig: ref sig2,
                generics: ref generics2,
                body: ref block2,
                ..
            }),
        ) => {
            let tokens1_stream = tokens1
                .as_ref()
                .unwrap()
                .create_token_stream()
                .to_tokenstream();
            let (old_args_tokens, old_args_span) =
                find_fn_header_arg_list(tokens1_stream, generics1.span)
                    .expect("failed to find arg list in item tokens");

            // First, try rewriting all the things we don't have special handling for.  If any of
            // these fails, bail out.
            let ok =
                // Item parts
                Rewrite::rewrite(attrs1, attrs2, rcx.borrow()) &&
                Rewrite::rewrite(id1, id2, rcx.borrow()) &&
                Rewrite::rewrite(span1, span2, rcx.borrow()) &&
                // ItemKind::Fn parts
                Rewrite::rewrite(&sig1.header.unsafety, &sig2.header.unsafety, rcx.borrow()) &&
                Rewrite::rewrite(&sig1.header.ext, &sig2.header.ext, rcx.borrow()) &&
                Rewrite::rewrite(generics1, generics2, rcx.borrow()) &&
                Rewrite::rewrite(block1, block2, rcx.borrow()) &&
                // FnDecl parts
                rewrite_arg_list_with_tokens(
                    &sig1.decl.inputs, &sig2.decl.inputs, old_args_tokens, old_args_span, rcx.borrow()) &&
                Rewrite::rewrite(&sig1.decl.output, &sig2.decl.output, rcx.borrow()) &&
                true;
            if !ok {
                return false;
            }

            // Now try to splice changes to vis, constness, unsafety, abi, and ident.  We use the
            // parser to find spans for all the old stuff.
            let src2: String = <Item as PrintParse>::to_string(new);
            let reparsed = Item::<ItemKind>::parse(rcx.session(), &src2);
            let reparsed_sig = expect!([&reparsed.kind]
                ItemKind::Fn(box Fn { ref sig, .. }) => sig);

            // The first two go in a specific order.  If multiple qualifiers are added (for
            // example, both `unsafe` and `extern`), we need to add them in the right order.

            if !vis1.kind.ast_equiv(&vis2.kind) {
                record_qualifier_rewrite(vis1.span, reparsed.vis.span, rcx.borrow());
            }

            if sig1.header.constness != sig2.header.constness {
                let sig1_const_span = match sig1.header.constness {
                    Const::Yes(sp) => sp,
                    Const::No => DUMMY_SP,
                };
                let reparsed_sig_const_span = match reparsed_sig.header.constness {
                    Const::Yes(sp) => sp,
                    Const::No => DUMMY_SP,
                };
                record_qualifier_rewrite(sig1_const_span, reparsed_sig_const_span, rcx.borrow());
            }

            if ident1 != ident2 {
                record_qualifier_rewrite(ident1.span, reparsed.ident.span, rcx.borrow());
            }

            true
        }

        (_, _) => {
            // Generic case, for items of the form "<vis> <struct/enum/etc> <ident>".
            let ok = Rewrite::rewrite(attrs1, attrs2, rcx.borrow())
                && Rewrite::rewrite(id1, id2, rcx.borrow())
                && Rewrite::rewrite(kind1, kind2, rcx.borrow())
                && Rewrite::rewrite(span1, span2, rcx.borrow())
                && true;
            if !ok {
                return false;
            }

            let src2: String = <Item as PrintParse>::to_string(new);
            let reparsed = Item::<ItemKind>::parse(rcx.session(), &src2);

            if !vis1.kind.ast_equiv(&vis2.kind) {
                record_qualifier_rewrite(vis1.span, reparsed.vis.span, rcx.borrow());
            }

            if ident1 != ident2 {
                record_qualifier_rewrite(ident1.span, reparsed.ident.span, rcx.borrow());
            }

            true
        }
    }
}
