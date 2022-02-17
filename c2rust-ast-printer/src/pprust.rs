use syntax::ast::{self, BlockCheckMode, PatKind, RangeEnd, RangeSyntax};
use syntax::ast::{SelfKind, GenericBound, TraitBoundModifier};
use syntax::ast::{Attribute, MacDelimiter, GenericArg, MacArgs};
use syntax::util::parser::{self, AssocOp, Fixity};
use syntax::util::comments;
use syntax::source_map::{self, SourceMap, Spanned};
use syntax::token::{self, BinOpToken, DelimToken, Nonterminal, Token, TokenKind};
use syntax::ptr::P;
use syntax::symbol::kw;
use syntax::tokenstream::{self, TokenStream, TokenTree};
use syntax::util::classify;

use syntax_pos::{self, BytePos, Span};

use std::borrow::Cow;

use crate::pp::{self, Breaks};
use crate::pp::Breaks::{Consistent, Inconsistent};
use crate::syntax_priv;

#[cfg(test)]
mod tests;

pub enum MacHeader<'a> {
    Path(&'a ast::Path),
    Keyword(&'static str),
}

pub enum AnnNode<'a> {
    Ident(&'a ast::Ident),
    Name(&'a ast::Name),
    Block(&'a ast::Block),
    Item(&'a ast::Item),
    SubItem(ast::NodeId),
    Expr(&'a ast::Expr),
    Pat(&'a ast::Pat),
    Crate(&'a ast::Crate),
}

pub trait PpAnn {
    fn pre(&self, _state: &mut State<'_>, _node: AnnNode<'_>) { }
    fn post(&self, _state: &mut State<'_>, _node: AnnNode<'_>) { }
}

#[derive(Copy, Clone)]
pub struct NoAnn;

impl PpAnn for NoAnn {}

pub struct Comments<'a> {
    cm: &'a SourceMap,
    comments: Vec<comments::Comment>,
    current: usize,
}

impl<'a> Comments<'a> {
    pub fn new(
        cm: &'a SourceMap,
        comments: Vec<comments::Comment>,
    ) -> Comments<'a> {
        Comments {
            cm,
            comments,
            current: 0,
        }
    }

    // pub fn parse(
    //     cm: &'a SourceMap,
    //     sess: &ParseSess,
    //     filename: FileName,
    //     input: String,
    // ) -> Comments<'a> {
    //     let comments = comments::gather_comments(sess, filename, input);
    //     Comments {
    //         cm,
    //         comments,
    //         current: 0,
    //     }
    // }

    pub fn next(&self) -> Option<comments::Comment> {
        self.comments.get(self.current).cloned()
    }

    pub fn trailing_comment(
        &mut self,
        span: syntax_pos::Span,
        next_pos: Option<BytePos>,
    ) -> Option<comments::Comment> {
        if let Some(cmnt) = self.next() {
            if cmnt.style != comments::Trailing { return None; }
            let span_line = self.cm.lookup_char_pos(span.hi());
            let comment_line = self.cm.lookup_char_pos(cmnt.pos);
            let next = next_pos.unwrap_or_else(|| cmnt.pos + BytePos(1));
            if span.hi() < cmnt.pos && cmnt.pos < next && span_line.line == comment_line.line {
                return Some(cmnt);
            }
        }

        None
    }
}

impl<'a> Extend<comments::Comment> for Comments<'a> {
    fn extend<I>(&mut self, iter: I)
        where I: IntoIterator<Item = comments::Comment>
    {
        self.comments.extend(iter);
    }
}
    

pub struct State<'a> {
    pub s: pp::Printer,
    comments: Option<Comments<'a>>,
    ann: &'a (dyn PpAnn+'a),
    is_expanded: bool
}

crate const INDENT_UNIT: usize = 4;

/// Requires you to pass an input filename and reader so that
/// it can scan the input text for comments to copy forward.
// pub fn print_crate<'a>(cm: &'a SourceMap,
//                        sess: &ParseSess,
//                        krate: &ast::Crate,
//                        filename: FileName,
//                        input: String,
//                        ann: &'a dyn PpAnn,
//                        is_expanded: bool) -> String {
//     let mut s = State {
//         s: pp::mk_printer(),
//         comments: Some(Comments::parse(cm, sess, filename, input)),
//         ann,
//         is_expanded,
//     };

//     if is_expanded && sess.injected_crate_name.try_get().is_some() {
//         // We need to print `#![no_std]` (and its feature gate) so that
//         // compiling pretty-printed source won't inject libstd again.
//         // However, we don't want these attributes in the AST because
//         // of the feature gate, so we fake them up here.

//         // `#![feature(prelude_import)]`
//         let pi_nested = attr::mk_nested_word_item(ast::Ident::with_dummy_span(sym::prelude_import));
//         let list = attr::mk_list_item(ast::Ident::with_dummy_span(sym::feature), vec![pi_nested]);
//         let fake_attr = attr::mk_attr_inner(list);
//         s.print_attribute(&fake_attr);

//         // Currently, in Rust 2018 we don't have `extern crate std;` at the crate
//         // root, so this is not needed, and actually breaks things.
//         if sess.edition == syntax_pos::edition::Edition::Edition2015 {
//             // `#![no_std]`
//             let no_std_meta = attr::mk_word_item(ast::Ident::with_dummy_span(sym::no_std));
//             let fake_attr = attr::mk_attr_inner(no_std_meta);
//             s.print_attribute(&fake_attr);
//         }
//     }

//     s.print_mod(&krate.module, &krate.attrs);
//     s.print_remaining_comments();
//     s.ann.post(&mut s, AnnNode::Crate(krate));
//     s.s.eof()
// }

pub fn to_string<F>(f: F) -> String where
    F: FnOnce(&mut State<'_>),
{
    let mut printer = State {
        s: pp::mk_printer(),
        comments: None,
        ann: &NoAnn,
        is_expanded: false
    };
    f(&mut printer);
    printer.s.eof()
}

pub fn to_string_with_comments<'a, F>(comments: Comments<'a>, f: F) -> String where
    F: FnOnce(&mut State<'_>)
{
    let mut printer = State {
        s: pp::mk_printer(),
        comments: Some(comments),
        ann: &NoAnn,
        is_expanded: false
    };
    f(&mut printer);
    printer.s.eof()
}


// This makes comma-separated lists look slightly nicer,
// and also addresses a specific regression described in issue #63896.
fn tt_prepend_space(tt: &TokenTree) -> bool {
    match tt {
        TokenTree::Token(token) => match token.kind {
            token::Comma => false,
            _ => true,
        }
        _ => true,
    }
}

fn binop_to_string(op: BinOpToken) -> &'static str {
    match op {
        token::Plus     => "+",
        token::Minus    => "-",
        token::Star     => "*",
        token::Slash    => "/",
        token::Percent  => "%",
        token::Caret    => "^",
        token::And      => "&",
        token::Or       => "|",
        token::Shl      => "<<",
        token::Shr      => ">>",
    }
}

pub fn literal_to_string(lit: token::Lit) -> String {
    let token::Lit { kind, symbol, suffix } = lit;
    let mut out = match kind {
        token::Byte          => format!("b'{}'", symbol),
        token::Char          => format!("'{}'", symbol),
        token::Str           => format!("\"{}\"", symbol),
        token::StrRaw(n)     => format!("r{delim}\"{string}\"{delim}",
                                        delim="#".repeat(n as usize),
                                        string=symbol),
        token::ByteStr       => format!("b\"{}\"", symbol),
        token::ByteStrRaw(n) => format!("br{delim}\"{string}\"{delim}",
                                        delim="#".repeat(n as usize),
                                        string=symbol),
        token::Integer       |
        token::Float         |
        token::Bool          |
        token::Err           => symbol.to_string(),
    };

    if let Some(suffix) = suffix {
        out.push_str(&suffix.as_str())
    }

    out
}

/// Print an ident from AST, `$crate` is converted into its respective crate name.
pub fn ast_ident_to_string(ident: ast::Ident, is_raw: bool) -> String {
    ident_to_string(ident.name, is_raw, Some(ident.span))
}

// AST pretty-printer is used as a fallback for turning AST structures into token streams for
// proc macros. Additionally, proc macros may stringify their input and expect it survive the
// stringification (especially true for proc macro derives written between Rust 1.15 and 1.30).
// So we need to somehow pretty-print `$crate` in a way preserving at least some of its
// hygiene data, most importantly name of the crate it refers to.
// As a result we print `$crate` as `crate` if it refers to the local crate
// and as `::other_crate_name` if it refers to some other crate.
// Note, that this is only done if the ident token is printed from inside of AST pretty-pringing,
// but not otherwise. Pretty-printing is the only way for proc macros to discover token contents,
// so we should not perform this lossy conversion if the top level call to the pretty-printer was
// done for a token stream or a single token.
fn ident_to_string(name: ast::Name, is_raw: bool, convert_dollar_crate: Option<Span>) -> String {
    if is_raw {
        format!("r#{}", name)
    } else {
        if name == kw::DollarCrate {
            if let Some(span) = convert_dollar_crate {
                let converted = span.ctxt().dollar_crate_name();
                return if converted.is_path_segment_keyword() {
                    converted.to_string()
                } else {
                    format!("::{}", converted)
                }
            }
        }
        name.to_string()
    }
}

/// Print the token kind precisely, without converting `$crate` into its respective crate name.
pub fn token_kind_to_string(tok: &TokenKind) -> String {
    token_kind_to_string_ext(tok, None)
}

fn token_kind_to_string_ext(tok: &TokenKind, convert_dollar_crate: Option<Span>) -> String {
    match *tok {
        token::Eq                   => "=".to_string(),
        token::Lt                   => "<".to_string(),
        token::Le                   => "<=".to_string(),
        token::EqEq                 => "==".to_string(),
        token::Ne                   => "!=".to_string(),
        token::Ge                   => ">=".to_string(),
        token::Gt                   => ">".to_string(),
        token::Not                  => "!".to_string(),
        token::Tilde                => "~".to_string(),
        token::OrOr                 => "||".to_string(),
        token::AndAnd               => "&&".to_string(),
        token::BinOp(op)            => binop_to_string(op).to_string(),
        token::BinOpEq(op)          => format!("{}=", binop_to_string(op)),

        /* Structural symbols */
        token::At                   => "@".to_string(),
        token::Dot                  => ".".to_string(),
        token::DotDot               => "..".to_string(),
        token::DotDotDot            => "...".to_string(),
        token::DotDotEq             => "..=".to_string(),
        token::Comma                => ",".to_string(),
        token::Semi                 => ";".to_string(),
        token::Colon                => ":".to_string(),
        token::ModSep               => "::".to_string(),
        token::RArrow               => "->".to_string(),
        token::LArrow               => "<-".to_string(),
        token::FatArrow             => "=>".to_string(),
        token::OpenDelim(token::Paren) => "(".to_string(),
        token::CloseDelim(token::Paren) => ")".to_string(),
        token::OpenDelim(token::Bracket) => "[".to_string(),
        token::CloseDelim(token::Bracket) => "]".to_string(),
        token::OpenDelim(token::Brace) => "{".to_string(),
        token::CloseDelim(token::Brace) => "}".to_string(),
        token::OpenDelim(token::NoDelim) |
        token::CloseDelim(token::NoDelim) => " ".to_string(),
        token::Pound                => "#".to_string(),
        token::Dollar               => "$".to_string(),
        token::Question             => "?".to_string(),
        token::SingleQuote          => "'".to_string(),

        /* Literals */
        token::Literal(lit) => literal_to_string(lit),

        /* Name components */
        token::Ident(s, is_raw)     => ident_to_string(s, is_raw, convert_dollar_crate),
        token::Lifetime(s)          => s.to_string(),

        /* Other */
        token::DocComment(s)        => s.to_string(),
        token::Eof                  => "<eof>".to_string(),
        token::Whitespace           => " ".to_string(),
        token::Comment              => "/* */".to_string(),
        token::Shebang(s)           => format!("/* shebang: {}*/", s),
        token::Unknown(s)           => s.to_string(),

        token::Interpolated(ref nt) => nonterminal_to_string(nt),
    }
}

/// Print the token precisely, without converting `$crate` into its respective crate name.
pub fn token_to_string(token: &Token) -> String {
    token_to_string_ext(token, false)
}

fn token_to_string_ext(token: &Token, convert_dollar_crate: bool) -> String {
    let convert_dollar_crate = if convert_dollar_crate { Some(token.span) } else { None };
    token_kind_to_string_ext(&token.kind, convert_dollar_crate)
}

pub fn nonterminal_to_string(nt: &Nonterminal) -> String {
    match *nt {
        token::NtExpr(ref e)        => expr_to_string(e),
        token::NtMeta(ref e)        => attr_item_to_string(e),
        token::NtTy(ref e)          => ty_to_string(e),
        token::NtPath(ref e)        => path_to_string(e),
        token::NtItem(ref e)        => item_to_string(e),
        token::NtBlock(ref e)       => block_to_string(e),
        token::NtStmt(ref e)        => stmt_to_string(e),
        token::NtPat(ref e)         => pat_to_string(e),
        token::NtIdent(e, is_raw)   => ast_ident_to_string(e, is_raw),
        token::NtLifetime(e)        => e.to_string(),
        token::NtLiteral(ref e)     => expr_to_string(e),
        token::NtTT(ref tree)       => tt_to_string(tree.clone()),
        token::NtImplItem(ref e)    => impl_item_to_string(e),
        token::NtTraitItem(ref e)   => trait_item_to_string(e),
        token::NtVis(ref e)         => vis_to_string(e),
        token::NtForeignItem(ref e) => foreign_item_to_string(e),
    }
}

pub fn ty_to_string(ty: &ast::Ty) -> String {
    to_string(|s| s.print_type(ty))
}

pub fn bounds_to_string(bounds: &[ast::GenericBound]) -> String {
    to_string(|s| s.print_type_bounds("", bounds))
}

pub fn pat_to_string(pat: &ast::Pat) -> String {
    to_string(|s| s.print_pat(pat))
}

pub fn expr_to_string(e: &ast::Expr) -> String {
    to_string(|s| s.print_expr(e))
}

pub fn tt_to_string(tt: tokenstream::TokenTree) -> String {
    to_string(|s| s.print_tt(tt, false))
}

pub fn tts_to_string(tokens: TokenStream) -> String {
    to_string(|s| s.print_tts(tokens, false))
}

pub fn stmt_to_string(stmt: &ast::Stmt) -> String {
    to_string(|s| s.print_stmt(stmt))
}

pub fn item_to_string(i: &ast::Item) -> String {
    to_string(|s| s.print_item(i))
}

pub fn impl_item_to_string(i: &ast::ImplItem) -> String {
    to_string(|s| s.print_impl_item(i))
}

pub fn trait_item_to_string(i: &ast::TraitItem) -> String {
    to_string(|s| s.print_trait_item(i))
}

pub fn generic_params_to_string(generic_params: &[ast::GenericParam]) -> String {
    to_string(|s| s.print_generic_params(generic_params))
}

pub fn path_to_string(p: &ast::Path) -> String {
    to_string(|s| s.print_path(p, false, 0))
}

pub fn path_segment_to_string(p: &ast::PathSegment) -> String {
    to_string(|s| s.print_path_segment(p, false))
}

pub fn vis_to_string(v: &ast::Visibility) -> String {
    to_string(|s| s.print_visibility(v))
}

pub fn block_to_string(blk: &ast::Block) -> String {
    to_string(|s| {
        // Containing cbox, will be closed by `print_block` at `}`.
        s.cbox(INDENT_UNIT);
        // Head-ibox, will be closed by `print_block` after `{`.
        s.ibox(0);
        s.print_block(blk)
    })
}

pub fn meta_list_item_to_string(li: &ast::NestedMetaItem) -> String {
    to_string(|s| s.print_meta_list_item(li))
}

fn attr_item_to_string(ai: &ast::AttrItem) -> String {
    to_string(|s| s.print_attr_item(ai, ai.path.span))
}

pub fn attribute_to_string(attr: &ast::Attribute) -> String {
    to_string(|s| s.print_attribute(attr))
}

pub fn param_to_string(arg: &ast::Param) -> String {
    to_string(|s| s.print_param(arg, false))
}

pub fn foreign_item_to_string(arg: &ast::ForeignItem) -> String {
    to_string(|s| s.print_foreign_item(arg))
}

pub fn visibility_qualified(vis: &ast::Visibility, s: &str) -> String {
    format!("{}{}", to_string(|s| s.print_visibility(vis)), s)
}

impl std::ops::Deref for State<'_> {
    type Target = pp::Printer;
    fn deref(&self) -> &Self::Target {
        &self.s
    }
}

impl std::ops::DerefMut for State<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.s
    }
}
