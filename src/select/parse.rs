use std::str::FromStr;
use std::vec;
use regex::Regex;
use rustc::session::Session;
use syntax::parse;
use syntax::parse::ParseSess;
use syntax::parse::parser::Parser;
use syntax::parse::token::{Token, DelimToken, Lit};
use syntax::symbol::Symbol;
use syntax::tokenstream::{TokenTree, TokenStream};

use pick_node::NodeKind;
use select::{SelectOp, Filter, AnyPattern, ItemLikeKind};

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
            sess: sess,
        }
    }

    fn eof(&self) -> bool {
        self.toks.as_slice().len() == 0
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

    fn last(mut self) -> PResult<()> {
        if let Some(tt) = self.peek() {
            fail!("expected end of input, but got {:?}", tt);
        } else {
            Ok(())
        }
    }


    fn token(&mut self) -> PResult<Token> {
        match self.take()? {
            TokenTree::Token(_, tok) => Ok(tok),
            TokenTree::Delimited(..) => fail!("expected token, but got delimited"),
        }
    }

    fn expect(&mut self, expect: &Token) -> PResult<()> {
        let actual = self.token()?;
        if &actual != expect {
            fail!("expected {:?}, but got {:?}", expect, actual);
        }
        Ok(())
    }

    fn maybe_expect(&mut self, expect: &Token) -> bool {
        let found = match self.peek() {
            Some(&TokenTree::Token(_, ref tok)) => tok == expect,
            _ => false,
        };
        if found {
            self.take().unwrap();
        }
        found
    }

    fn name(&mut self) -> PResult<Symbol> {
        match self.token()? {
            Token::Ident(i) => Ok(i.name),
            t => fail!("expected name, but got {:?}", t),
        }
    }

    fn parens_raw(&mut self) -> PResult<TokenStream> {
        match self.take()? {
            TokenTree::Delimited(_, d) => {
                if d.delim != DelimToken::Paren {
                    fail!("expected parens, but got {:?}", d.delim);
                }
                Ok(d.tts.into())
            },
            TokenTree::Token(_, tok) => fail!("expected parens, but got {:?}", tok),
        }
    }

    fn parens(&mut self) -> PResult<Stream<'a>> {
        self.parens_raw().map(|ts| Stream::new(self.sess, ts.into_trees().collect()))
    }

    fn maybe_parens(&mut self) -> Option<Stream<'a>> {
        let has_parens = match self.peek() {
            Some(&TokenTree::Delimited(_, ref d)) => d.delim == DelimToken::Paren,
            _ => false,
        };

        if has_parens {
            Some(self.parens().unwrap())
        } else {
            None
        }
    }

    fn lit(&mut self) -> PResult<Lit> {
        match self.token()? {
            Token::Literal(lit, _) => Ok(lit),
            t => fail!("expected literal, but got {:?}", t),
        }
    }


    fn filter(&mut self) -> PResult<Filter> {
        self.filter_or()
    }

    fn filter_or(&mut self) -> PResult<Filter> {
        let mut parts = vec![self.filter_and()?];
        while self.maybe_expect(&Token::OrOr) {
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
        while self.maybe_expect(&Token::AndAnd) {
            parts.push(self.filter_single()?);
        }

        if parts.len() == 1 {
            Ok(parts.into_iter().next().unwrap())
        } else {
            Ok(Filter::And(parts))
        }
    }

    fn filter_single(&mut self) -> PResult<Filter> {
        if self.maybe_expect(&Token::Not) {
            let filt = self.filter_single()?;
            Ok(Filter::Not(Box::new(filt)))
        } else if let Some(mut inner) = self.maybe_parens() {
            let filt = inner.filter()?;
            inner.last();
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
                },

                "item_kind" => {
                    let mut inner = self.parens()?;
                    let subkind_str = inner.name()?;
                    inner.last()?;

                    let subkind = match ItemLikeKind::from_str(&subkind_str.as_str()) {
                        Ok(k) => k,
                        Err(_) => fail!("invalid itemlike kind `{}`", subkind_str.as_str()),
                    };
                    Ok(Filter::ItemKind(subkind))
                },

                "pub" => {
                    Ok(Filter::Public)
                },

                "name" => {
                    let mut inner = self.parens()?;
                    let lit = inner.lit()?;
                    inner.last()?;

                    let s = match lit {
                        Lit::Str_(s) => s,
                        Lit::StrRaw(s, _) => s,
                        l => fail!("expected string literal, but got {:?}", l),
                    };
                    let r = match Regex::new(&s.as_str()) {
                        Ok(r) => r,
                        Err(e) => fail!("invalid regex: {}", e),
                    };
                    Ok(Filter::Name(r))
                },

                "has_attr" => {
                    let mut inner = self.parens()?;
                    let name = inner.name()?;
                    inner.last()?;
                    Ok(Filter::HasAttr(name))
                },

                "match_expr" => {
                    let ts = self.parens_raw()?;

                    let mut p = Parser::new(self.sess, ts, None, false, false);
                    let x = p.parse_expr()
                        .map_err(|e| format!("error parsing expr: {}", e.message()))?;
                    p.expect(&Token::Eof)
                        .map_err(|e| format!("error parsing stmt: {}", e.message()))?;

                    Ok(Filter::Matches(AnyPattern::Expr(x)))
                },

                "match_ty" => {
                    let ts = self.parens_raw()?;

                    let mut p = Parser::new(self.sess, ts, None, false, false);
                    let x = p.parse_ty()
                        .map_err(|e| format!("error parsing ty: {}", e.message()))?;
                    p.expect(&Token::Eof)
                        .map_err(|e| format!("error parsing stmt: {}", e.message()))?;

                    Ok(Filter::Matches(AnyPattern::Ty(x)))
                },

                "match_stmt" => {
                    let ts = self.parens_raw()?;

                    let mut p = Parser::new(self.sess, ts, None, false, false);
                    let x = match p.parse_full_stmt(false) {
                        Ok(Some(x)) => x,
                        Ok(None) => fail!("expected stmt"),
                        Err(e) => fail!("error parsing stmt: {}", e.message()),
                    };
                    p.expect(&Token::Eof)
                        .map_err(|e| format!("error parsing stmt: {}", e.message()))?;

                    Ok(Filter::Matches(AnyPattern::Stmt(x)))
                },

                "marked" => {
                    let mut inner = self.parens()?;
                    let label = inner.name()?;
                    inner.last()?;
                    Ok(Filter::Marked(label))
                },

                "any_child" => {
                    let mut inner = self.parens()?;
                    let filt = inner.filter()?;
                    inner.last();
                    Ok(Filter::AnyChild(Box::new(filt)))
                },

                "every_child" => {
                    let mut inner = self.parens()?;
                    let filt = inner.filter()?;
                    inner.last();
                    Ok(Filter::AllChild(Box::new(filt)))
                },

                "any_desc" => {
                    let mut inner = self.parens()?;
                    let filt = inner.filter()?;
                    inner.last();
                    Ok(Filter::AnyDesc(Box::new(filt)))
                },

                "every_desc" => {
                    let mut inner = self.parens()?;
                    let filt = inner.filter()?;
                    inner.last();
                    Ok(Filter::AllDesc(Box::new(filt)))
                },

                name => fail!("unknown filter op `{}`", name),
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
            },

            "crate" => {
                SelectOp::Crate
            },

            "child" => {
                let mut inner = self.parens()?;
                let filter = inner.filter()?;
                inner.last()?;
                SelectOp::ChildMatch(filter)
            },

            "desc" => {
                let mut inner = self.parens()?;
                let filter = inner.filter()?;
                inner.last()?;
                SelectOp::DescMatch(filter)
            },

            "filter" => {
                let mut inner = self.parens()?;
                let filter = inner.filter()?;
                inner.last()?;
                SelectOp::Filter(filter)
            },

            name => fail!("unknown select op `{}`", name),
        };

        self.expect(&Token::Semi)?;
        Ok(op)
    }
}


pub fn parse(sess: &Session, src: &str) -> Vec<SelectOp> {
    let fm = sess.codemap().new_filemap("<select>".to_owned(),
                                        src.to_owned());
    eprintln!("src = {:?}", src);
    eprintln!("fm = {:?}", fm);
    let ts = parse::filemap_to_stream(&sess.parse_sess, fm, None);
    eprintln!("tokens = {:?}", ts);

    let mut stream = Stream::new(&sess.parse_sess, ts.into_trees().collect());
    let mut ops = Vec::new();
    while !stream.eof() {
        ops.push(stream.select_op().unwrap());
    }
    ops
}
