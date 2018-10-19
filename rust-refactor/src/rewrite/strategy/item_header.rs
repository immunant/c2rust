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
use syntax::ast::*;
use syntax::source_map::{Span, BytePos};
use syntax::parse::PResult;
use syntax::parse::parser::Parser;
use syntax::parse::token::{Token, DelimToken};
use syntax::symbol::keywords;
use syntax::tokenstream::{TokenStream, ThinTokenStream, TokenTree};

use driver;
use rewrite::{Rewrite, RewriteCtxtRef, TextAdjust};
use rewrite::base::{describe, rewrite_seq_comma_sep};
use rewrite::strategy::print::PrintParse;

struct FnHeaderSpans {
    vis: Span,
    constness: Span,
    unsafety: Span,
    abi: Span,
    ident: Span,
}

fn start_point(sp: Span) -> Span {
    sp.with_hi(sp.lo())
}

fn span_empty(sp: Span) -> bool {
    sp.lo() == sp.hi()
}

fn find_fn_header_spans<'a>(p: &mut Parser<'a>) -> PResult<'a, FnHeaderSpans> {
    // Skip over any attributes that were included in the token stream.
    loop {
        if matches!([p.token] Token::DocComment(..)) {
            p.bump();
        } else if matches!([p.token] Token::Pound) {
            // I don't think we should ever see inner attributes inside `item.tokens`, but allow
            // them just in case.
            p.parse_attribute(true)?;
        } else {
            break;
        }
    }

    let spanned_vis = p.parse_visibility(false)?;
    let vis = if spanned_vis.node != VisibilityKind::Inherited {
        spanned_vis.span
    } else {
        // `Inherited` visibility is implicit - there are no actual tokens.  Insert visibility just
        // before the next token.
        start_point(p.span)
    };

    let constness = if p.eat_keyword(keywords::Const) {
        p.prev_span
    } else {
        start_point(p.span)
    };

    let unsafety = if p.eat_keyword(keywords::Unsafe) {
        p.prev_span
    } else {
        start_point(p.span)
    };

    let abi = if p.eat_keyword(keywords::Extern) {
        let extern_span = p.prev_span;
        if matches!([p.token] Token::Literal(..)) {
            // Just assume it's a valid abi string token.  If it wasn't, these tokens wouldn't have
            // parsed as an item to begin with.
            p.bump();
            extern_span.to(p.prev_span)
        } else {
            // Implicitly `extern "C"`.
            extern_span
        }
    } else {
        start_point(p.span)
    };

    p.expect(&Token::Ident(keywords::Fn.ident(), false))?;

    p.parse_ident()?;
    let ident = p.prev_span;

    Ok(FnHeaderSpans { vis, constness, unsafety, abi, ident })
}

struct ItemHeaderSpans {
    vis: Span,
    ident: Span,
}

/// Generic parsing function for item headers of the form "<vis> <struct/enum/etc> <ident>".
fn find_item_header_spans<'a>(p: &mut Parser<'a>) -> PResult<'a, ItemHeaderSpans> {
    // Skip over any attributes that were included in the token stream.
    loop {
        if matches!([p.token] Token::DocComment(..)) {
            p.bump();
        } else if matches!([p.token] Token::Pound) {
            // I don't think we should ever see inner attributes inside `item.tokens`, but allow
            // them just in case.
            p.parse_attribute(true)?;
        } else {
            break;
        }
    }

    let spanned_vis = p.parse_visibility(false)?;
    let vis = if spanned_vis.node != VisibilityKind::Inherited {
        spanned_vis.span
    } else {
        // `Inherited` visibility is implicit - there are no actual tokens.  Insert visibility just
        // before the next token.
        start_point(p.span)
    };

    let kws = &[
        keywords::Static,
        keywords::Const,
        keywords::Fn,
        keywords::Mod,
        keywords::Type,
        keywords::Enum,
        keywords::Struct,
        keywords::Union,
        keywords::Trait,
    ];

    for (i, &kw) in kws.iter().enumerate() {
        if i < kws.len() - 1 {
            if p.eat_keyword(kw) {
                break;
            }
        } else {
            // Use `expect` for the last one so we produce a parse error on "none of the above".
            p.expect(&Token::Ident(kw.ident(), false))?;
            break;
        }
    }

    p.parse_ident()?;
    let ident = p.prev_span;

    Ok(ItemHeaderSpans { vis, ident })
}

fn find_fn_header_arg_list(ts: TokenStream,
                           generics_span: Span) -> Option<(ThinTokenStream, Span)> {
    // Take the body of the first paren-delimited subtree that's strictly after `generics_span`.
    ts.trees().filter_map(|tt| {
        match tt {
            TokenTree::Delimited(sp, ref d)
                    if d.delim == DelimToken::Paren && sp.lo() >= generics_span.hi() => {
                let inner_lo = sp.lo() + BytePos(d.delim.len() as u32);
                let inner_hi = sp.hi() - BytePos(d.delim.len() as u32);
                let inner_span = Span::new(inner_lo, inner_hi, sp.ctxt());
                Some((d.tts.clone(), inner_span))
            },
            _ => None,
        }
    }).next()
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

    rcx.record(old_span, src_span, vec![], TextAdjust::None);
}

fn rewrite_arg_list_with_tokens(old: &[Arg],
                                new: &[Arg],
                                args_tokens: TokenStream,
                                args_span: Span,
                                rcx: RewriteCtxtRef) -> bool {
    info!("old arg list = {:?}", old);
    info!("new arg list = {:?}", new);

    // Find commas that separate arguments.  This is harder than it should be because type
    // argument lists (`<...>`) aren't `Delimited` like parens etc.  So instead of just looking for
    // commas at top level, we have to find commas that aren't included in any argument span.
    let mut comma_spans = Vec::with_capacity(old.len());
    let mut tt_iter = args_tokens.into_trees();
    for (i, old_arg) in old.iter().enumerate() {
        // First, run `tt_iter` forward until we're past `old_arg`.
        assert!(old_arg.ty.span.hi() > old_arg.pat.span.hi());  // sanity check
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
                    if past_arg && matches!([tt] TokenTree::Token(_, Token::Comma)) {
                        // Found the comma following the current arg.
                        comma_spans.push(tt.span());
                        break;
                    }
                },

                None => {
                    if is_last {
                        // It's okay if the trailing comma is missing.  Note we don't check for
                        // `past_arg` because it's possible that the arg includes the last token of
                        // the stream.
                        break;
                    } else {
                        panic!("not enough commas in arg list token stream");
                    }
                },
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
    rewrite_seq_comma_sep(old, new, &spans_with_commas, args_span, has_trailing_comma, rcx)
}

pub fn rewrite(old: &Item, new: &Item, mut rcx: RewriteCtxtRef) -> bool {
    let &Item { ident: ref ident1, attrs: ref attrs1, id: ref id1, node: ref node1,
                vis: ref vis1, span: ref span1, tokens: ref tokens1 } = old;
    let &Item { ident: ref ident2, attrs: ref attrs2, id: ref id2, node: ref node2,
                vis: ref vis2, span: ref span2, tokens: ref _tokens2 } = new;

    // We can't do anything without tokens to parse.  (This is not quite true - we could
    // pretty-print and reparse `old`.  But that's a pain, so just require tokens instead.)
    if tokens1.is_none() {
        return false;
    }

    match (node1, node2) {
        (&ItemKind::Fn(ref decl1, ref unsafety1, ref constness1, ref abi1, ref generics1, ref block1),
         &ItemKind::Fn(ref decl2, ref unsafety2, ref constness2, ref abi2, ref generics2, ref block2)) => {

            let FnDecl { inputs: inputs1, output: output1, variadic: variadic1 } = decl1 as &_;
            let FnDecl { inputs: inputs2, output: output2, variadic: variadic2 } = decl2 as &_;

            let (old_args_tokens, old_args_span) =
                find_fn_header_arg_list(tokens1.as_ref().unwrap().clone(), generics1.span)
                    .expect("failed to find arg list in item tokens");

            // First, try rewriting all the things we don't have special handling for.  If any of
            // these fails, bail out.
            let ok =
                // Item parts
                Rewrite::rewrite(attrs1, attrs2, rcx.borrow()) &&
                Rewrite::rewrite(id1, id2, rcx.borrow()) &&
                Rewrite::rewrite(span1, span2, rcx.borrow()) &&
                // ItemKind::Fn parts
                Rewrite::rewrite(generics1, generics2, rcx.borrow()) &&
                Rewrite::rewrite(block1, block2, rcx.borrow()) &&
                // FnDecl parts
                rewrite_arg_list_with_tokens(
                    inputs1, inputs2, old_args_tokens.into(), old_args_span, rcx.borrow()) &&
                Rewrite::rewrite(output1, output2, rcx.borrow()) &&
                Rewrite::rewrite(variadic1, variadic2, rcx.borrow()) &&
                true;
            if !ok {
                return false;
            }

            // Now try to splice changes to vis, constness, unsafety, abi, and ident.  We use the
            // parser to find spans for all the old stuff.
            //
            // We could recover from parse errors by bailing on the rewrite (returning `true`), but
            // it's easier to just panic.
            let tts1 = tokens1.as_ref().unwrap().trees().collect::<Vec<_>>();
            let spans1 = driver::run_parser_tts(rcx.session(), tts1, find_fn_header_spans);

            let src2: String = <Item as PrintParse>::to_string(new);
            let spans2 = driver::run_parser(rcx.session(), &src2, find_fn_header_spans);


            // The first four go in a specific order.  If multiple qualifiers are added (for
            // example, both `unsafe` and `extern`), we need to add them in the right order.

            if vis1.node != vis2.node {
                record_qualifier_rewrite(spans1.vis, spans2.vis, rcx.borrow());
            }

            if constness1.node != constness2.node {
                record_qualifier_rewrite(spans1.constness, spans2.constness, rcx.borrow());
            }

            if unsafety1 != unsafety2 {
                record_qualifier_rewrite(spans1.unsafety, spans2.unsafety, rcx.borrow());
            }

            if abi1 != abi2 {
                record_qualifier_rewrite(spans1.abi, spans2.abi, rcx.borrow());
            }

            if ident1 != ident2 {
                record_qualifier_rewrite(spans1.ident, spans2.ident, rcx.borrow());
            }

            true
        },

        (_, _) => {
            // Generic case, for items of the form "<vis> <struct/enum/etc> <ident>".
            let ok =
                Rewrite::rewrite(attrs1, attrs2, rcx.borrow()) &&
                Rewrite::rewrite(id1, id2, rcx.borrow()) &&
                Rewrite::rewrite(node1, node2, rcx.borrow()) &&
                Rewrite::rewrite(span1, span2, rcx.borrow()) &&
                true;
            if !ok {
                return false;
            }

            let tts1 = tokens1.as_ref().unwrap().trees().collect::<Vec<_>>();
            let spans1 = match driver::try_run_parser_tts(rcx.session(), tts1,
                                                          find_item_header_spans) {
                Some(x) => x,
                None => return false,
            };

            let src2: String = <Item as PrintParse>::to_string(new);
            let spans2 = match driver::try_run_parser(rcx.session(), &src2,
                                                      find_item_header_spans) {
                Some(x) => x,
                None => return false,
            };


            if vis1.node != vis2.node {
                record_qualifier_rewrite(spans1.vis, spans2.vis, rcx.borrow());
            }

            if ident1 != ident2 {
                record_qualifier_rewrite(spans1.ident, spans2.ident, rcx.borrow());
            }

            true
        },
    }
}
