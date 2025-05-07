use log::debug;
use regex::Regex;
use rustc_ast::token::{Delimiter, Lit, LitKind, Token, TokenKind};
use rustc_ast::tokenstream::{TokenStream, TokenTree};
use rustc_ast::{ExprKind, Path};
use rustc_parse::parser::{AttemptLocalParseRecovery, Parser};
use rustc_session::parse::ParseSess;
use rustc_session::Session;
use rustc_span::symbol::Symbol;
use rustc_span::FileName;
use std::mem;
use std::str::FromStr;
use std::vec;

use crate::ast_manip::remove_paren;
use crate::pick_node::NodeKind;
use crate::select::{AnyPattern, Filter, ItemLikeKind, SelectOp};

type PResult<T> = Result<T, String>;

macro_rules! fail {
    ($($args:tt)*) => {
        return Err(format!($($args)*))
    };
}

struct Stream<'a> {
    toks: vec::IntoIter<TokenTree>,
    sess: &'a ParseSess,
}

impl<'a> Stream<'a> {
    fn new(sess: &'a ParseSess, toks: Vec<TokenTree>) -> Stream<'a> {
        Stream {
            toks: toks.into_iter(),
            sess,
        }
    }

    fn eof(&self) -> bool {
        self.toks.as_slice().is_empty()
    }

    fn peek(&self) -> Option<&TokenTree> {
        self.toks.as_slice().get(0)
    }

    fn take(&mut self) -> PResult<TokenTree> {
        match self.toks.next() {
            Some(x) => Ok(x),
            None => fail!("expected token tree, but got eof"),
        }
    }

    fn last(self) -> PResult<()> {
        if let Some(tt) = self.peek() {
            fail!("expected end of input, but got {:?}", tt);
        } else {
            Ok(())
        }
    }

    fn token(&mut self) -> PResult<Token> {
        match self.take()? {
            TokenTree::Token(tok, _) => Ok(tok),
            TokenTree::Delimited(..) => fail!("expected token, but got delimited"),
        }
    }

    fn expect(&mut self, expect: &TokenKind) -> PResult<()> {
        let actual = self.token()?;
        if &actual.kind != expect {
            fail!("expected {:?}, but got {:?}", expect, actual);
        }
        Ok(())
    }

    fn maybe_expect(&mut self, expect: &TokenKind) -> bool {
        let found = match self.peek() {
            Some(&TokenTree::Token(ref tok, _)) => &tok.kind == expect,
            _ => false,
        };
        if found {
            self.take().unwrap();
        }
        found
    }

    fn name(&mut self) -> PResult<Symbol> {
        match self.token()?.kind {
            TokenKind::Ident(i, _) => Ok(i),
            t => fail!("expected name, but got {:?}", t),
        }
    }

    fn parens_raw(&mut self) -> PResult<TokenStream> {
        match self.take()? {
            TokenTree::Delimited(_, delim, tts) => {
                if delim != Delimiter::Parenthesis {
                    fail!("expected parens, but got {:?}", delim);
                }
                Ok(tts)
            }
            TokenTree::Token(tok, _) => fail!("expected parens, but got {:?}", tok),
        }
    }

    fn parens(&mut self) -> PResult<Stream<'a>> {
        self.parens_raw()
            .map(|ts| Stream::new(self.sess, ts.into_trees().collect()))
    }

    fn maybe_parens(&mut self) -> Option<Stream<'a>> {
        let has_parens = match self.peek() {
            Some(&TokenTree::Delimited(_, Delimiter::Parenthesis, _)) => true,
            _ => false,
        };

        if has_parens {
            Some(self.parens().unwrap())
        } else {
            None
        }
    }

    fn lit(&mut self) -> PResult<Lit> {
        match self.token()?.kind {
            TokenKind::Literal(lit) => Ok(lit),
            t => fail!("expected literal, but got {:?}", t),
        }
    }

    fn path(&mut self) -> PResult<Path> {
        let ts = mem::replace(&mut self.toks, Vec::new().into_iter());
        let mut p = Parser::new(self.sess, ts.collect(), false, None);
        let path_expr = p
            .parse_expr()
            .map_err(|e| format!("error parsing path as expr: {:?}", e.message))?;
        let path = match path_expr.into_inner().kind {
            ExprKind::Path(None, p) => p,
            kind @ _ => return Err(format!("error converting expr to path: {:?}", kind)),
        };
        self.toks = p
            .parse_all_token_trees()
            .map_err(|e| format!("error parsing path: {:?}", e.message))?
            .into_iter();
        Ok(path)
    }

    fn filter(&mut self) -> PResult<Filter> {
        self.filter_or()
    }

    fn filter_or(&mut self) -> PResult<Filter> {
        let mut parts = vec![self.filter_and()?];
        while self.maybe_expect(&TokenKind::OrOr) {
            parts.push(self.filter_and()?);
        }

        if parts.len() == 1 {
            Ok(parts.into_iter().next().unwrap())
        } else {
            Ok(Filter::Or(parts))
        }
    }

    fn filter_and(&mut self) -> PResult<Filter> {
        let mut parts = vec![self.filter_single()?];
        while self.maybe_expect(&TokenKind::AndAnd) {
            parts.push(self.filter_single()?);
        }

        if parts.len() == 1 {
            Ok(parts.into_iter().next().unwrap())
        } else {
            Ok(Filter::And(parts))
        }
    }

    fn filter_single(&mut self) -> PResult<Filter> {
        if self.maybe_expect(&TokenKind::Not) {
            let filt = self.filter_single()?;
            Ok(Filter::Not(Box::new(filt)))
        } else if let Some(mut inner) = self.maybe_parens() {
            let filt = inner.filter()?;
            inner.last()?;
            Ok(filt)
        } else {
            match &self.name()?.as_str() as &str {
                "kind" => {
                    let mut inner = self.parens()?;
                    let kind_str = inner.name()?;
                    inner.last()?;

                    let kind = match NodeKind::from_str(&kind_str.as_str()) {
                        Ok(k) => k,
                        Err(_) => fail!("invalid node kind `{}`", kind_str.as_str()),
                    };
                    Ok(Filter::Kind(kind))
                }

                "item_kind" => {
                    let mut inner = self.parens()?;
                    let subkind_str = inner.name()?;
                    inner.last()?;

                    let subkind = match ItemLikeKind::from_str(&subkind_str.as_str()) {
                        Ok(k) => k,
                        Err(_) => fail!("invalid itemlike kind `{}`", subkind_str.as_str()),
                    };
                    Ok(Filter::ItemKind(subkind))
                }

                "path" => {
                    let mut inner = self.parens()?;
                    let path = inner.path()?;
                    inner.last()?;
                    Ok(Filter::PathPrefix(0, Box::new(path)))
                }

                "path_prefix" => {
                    let mut inner = self.parens()?;
                    let seg_count_lit = inner.lit()?;
                    inner.expect(&TokenKind::Comma)?;
                    let path = inner.path()?;
                    inner.last()?;

                    let seg_count = match seg_count_lit.kind {
                        LitKind::Integer => match usize::from_str(&seg_count_lit.symbol.as_str()) {
                            Ok(i) => i,
                            Err(e) => fail!("error parsing integer: {}", e),
                        },
                        l => fail!("expected integer, but got {:?}", l),
                    };

                    Ok(Filter::PathPrefix(seg_count, Box::new(path)))
                }

                "pub" => Ok(Filter::Public),

                "mut" => Ok(Filter::Mutable),

                "name" => {
                    let mut inner = self.parens()?;
                    let lit = inner.lit()?;
                    inner.last()?;

                    let s = match lit.kind {
                        LitKind::Str | LitKind::StrRaw(_) => lit.symbol,
                        l => fail!("expected string literal, but got {:?}", l),
                    };
                    // First, make sure `s` parses as a regex on its own
                    let _ = match Regex::new(&s.as_str()) {
                        Ok(r) => r,
                        Err(e) => fail!("invalid regex: {}", e),
                    };
                    // Then, add ^ ... $ so the regex has to match the entire item name
                    let r = Regex::new(&format!("^({})$", s.as_str())).unwrap();
                    Ok(Filter::Name(r))
                }

                "has_attr" => {
                    let mut inner = self.parens()?;
                    let name = inner.name()?;
                    inner.last()?;
                    Ok(Filter::HasAttr(name))
                }

                "match_expr" => {
                    let ts = self.parens_raw()?;

                    let mut p = Parser::new(self.sess, ts, false, None);
                    let mut x = p
                        .parse_expr()
                        .map_err(|e| format!("error parsing expr: {:?}", e.message))?;
                    p.expect(&TokenKind::Eof)
                        .map_err(|e| format!("error parsing expr: {:?}", e.message))?;

                    remove_paren(&mut x);
                    Ok(Filter::Matches(AnyPattern::Expr(x)))
                }

                "match_pat" => {
                    let ts = self.parens_raw()?;

                    let mut p = Parser::new(self.sess, ts, false, None);
                    let mut x = p
                        .parse_pat_no_top_alt(None)
                        .map_err(|e| format!("error parsing pat: {:?}", e.message))?;
                    p.expect(&TokenKind::Eof)
                        .map_err(|e| format!("error parsing pat: {:?}", e.message))?;

                    remove_paren(&mut x);
                    Ok(Filter::Matches(AnyPattern::Pat(x)))
                }

                "match_ty" => {
                    let ts = self.parens_raw()?;

                    let mut p = Parser::new(self.sess, ts, false, None);
                    let mut x = p
                        .parse_ty()
                        .map_err(|e| format!("error parsing ty: {:?}", e.message))?;
                    p.expect(&TokenKind::Eof)
                        .map_err(|e| format!("error parsing ty: {:?}", e.message))?;

                    remove_paren(&mut x);
                    Ok(Filter::Matches(AnyPattern::Ty(x)))
                }

                "match_stmt" => {
                    let ts = self.parens_raw()?;

                    let mut p = Parser::new(self.sess, ts, false, None);
                    let mut x = match p.parse_full_stmt(AttemptLocalParseRecovery::Yes) {
                        Ok(Some(x)) => x,
                        Ok(None) => fail!("expected stmt"),
                        Err(e) => fail!("error parsing stmt: {:?}", e.message),
                    };
                    if let TokenKind::Semi = p.token.kind {
                        p.bump();
                    }
                    p.expect(&TokenKind::Eof)
                        .map_err(|e| format!("error parsing stmt: {:?}", e.message))?;

                    remove_paren(&mut x);
                    Ok(Filter::Matches(AnyPattern::Stmt(x)))
                }

                "marked" => {
                    let mut inner = self.parens()?;
                    let label = inner.name()?;
                    inner.last()?;
                    Ok(Filter::Marked(label))
                }

                "any_child" => {
                    let mut inner = self.parens()?;
                    let filt = inner.filter()?;
                    inner.last()?;
                    Ok(Filter::AnyChild(Box::new(filt)))
                }

                "every_child" => {
                    let mut inner = self.parens()?;
                    let filt = inner.filter()?;
                    inner.last()?;
                    Ok(Filter::AllChild(Box::new(filt)))
                }

                "any_desc" => {
                    let mut inner = self.parens()?;
                    let filt = inner.filter()?;
                    inner.last()?;
                    Ok(Filter::AnyDesc(Box::new(filt)))
                }

                "every_desc" => {
                    let mut inner = self.parens()?;
                    let filt = inner.filter()?;
                    inner.last()?;
                    Ok(Filter::AllDesc(Box::new(filt)))
                }

                name => {
                    // Shorthand for `kind(x)` and `item_kind(x)`
                    if let Ok(kind) = NodeKind::from_str(name) {
                        Ok(Filter::Kind(kind))
                    } else if let Ok(kind) = ItemLikeKind::from_str(name) {
                        Ok(Filter::ItemKind(kind))
                    } else {
                        fail!("unknown filter op `{}`", name)
                    }
                }
            }
        }
    }

    fn select_op(&mut self) -> PResult<SelectOp> {
        let op = match &self.name()?.as_str() as &str {
            "marked" => {
                let mut inner = self.parens()?;
                let label = inner.name()?;
                inner.last()?;
                SelectOp::Marked(label)
            }

            "mark" => {
                let mut inner = self.parens()?;
                let label = inner.name()?;
                inner.last()?;
                SelectOp::Mark(label)
            }

            "unmark" => {
                let mut inner = self.parens()?;
                let label = inner.name()?;
                inner.last()?;
                SelectOp::Unmark(label)
            }

            "reset" => SelectOp::Reset,

            "crate" => SelectOp::Crate,

            "item" => {
                let mut inner = self.parens()?;
                let path = inner.path()?;
                inner.last()?;
                SelectOp::Item(path)
            }

            "child" => {
                let mut inner = self.parens()?;
                let filter = inner.filter()?;
                inner.last()?;
                SelectOp::ChildMatch(filter)
            }

            "desc" => {
                let mut inner = self.parens()?;
                let filter = inner.filter()?;
                inner.last()?;
                SelectOp::DescMatch(filter)
            }

            "filter" => {
                let mut inner = self.parens()?;
                let filter = inner.filter()?;
                inner.last()?;
                SelectOp::Filter(filter)
            }

            "first" => SelectOp::First,

            "last" => SelectOp::Last,

            name => fail!("unknown select op `{}`", name),
        };

        self.expect(&TokenKind::Semi)?;
        Ok(op)
    }
}

pub fn parse(sess: &Session, src: &str) -> Vec<SelectOp> {
    debug!("src = {:?}", src);
    let ts = rustc_parse::parse_stream_from_source_str(
        FileName::macro_expansion_source_code(src),
        src.to_string(),
        &sess.parse_sess,
        None,
    );
    debug!("tokens = {:?}", ts);

    let mut stream = Stream::new(&sess.parse_sess, ts.into_trees().collect());
    let mut ops = Vec::new();
    while !stream.eof() {
        ops.push(stream.select_op().unwrap());
    }
    ops
}
