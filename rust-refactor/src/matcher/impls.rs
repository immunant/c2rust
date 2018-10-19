//! `TryMatch` impls, to support the `matcher` module.
use std::rc::Rc;
use rustc_target::spec::abi::Abi;
use syntax::ThinVec;
use syntax::ast::*;
use syntax::source_map::{Span, Spanned};
use syntax::ext::hygiene::SyntaxContext;
use syntax::parse::token::{Token, DelimToken, Nonterminal};
use syntax::ptr::P;
use syntax::tokenstream::{TokenTree, Delimited, TokenStream, ThinTokenStream};

use ast_manip::util::macro_name;
use matcher::{self, TryMatch, MatchCtxt};


impl TryMatch for Ident {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        if mcx.maybe_capture_ident(self, target)? {
            return Ok(());
        } else if mcx.maybe_capture_label(self, target)? {
            return Ok(());
        }

        if self == target {
            Ok(())
        } else {
            Err(matcher::Error::SymbolMismatch)
        }
    }
}

impl TryMatch for Path {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        if mcx.maybe_capture_path(self, target)? {
            return Ok(());
        }

        default_try_match_path(self, target, mcx)
    }
}

impl TryMatch for Expr {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        if mcx.maybe_capture_expr(self, target)? {
            return Ok(());
        }

        if let ExprKind::Mac(ref mac) = self.node {
            let name = macro_name(mac);
            return match &name.as_str() as &str {
                "marked" => mcx.do_marked(&mac.node.tts,
                                          |p| p.parse_expr().map(|p| p.into_inner()),
                                          target),
                "def" => mcx.do_def_expr(&mac.node.tts, target),
                "typed" => mcx.do_typed(&mac.node.tts,
                                        |p| p.parse_expr().map(|p| p.into_inner()),
                                        target),
                _ => Err(matcher::Error::BadSpecialPattern(name)),
            };
        }

        default_try_match_expr(self, target, mcx)
    }
}

impl TryMatch for Pat {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        if mcx.maybe_capture_pat(self, target)? {
            return Ok(());
        }

        if let PatKind::Mac(ref mac) = self.node {
            let name = macro_name(mac);
            return match &name.as_str() as &str {
                "marked" => mcx.do_marked(&mac.node.tts,
                                          |p| p.parse_pat().map(|p| p.into_inner()),
                                          target),
                "typed" => mcx.do_typed(&mac.node.tts,
                                        |p| p.parse_pat().map(|p| p.into_inner()),
                                        target),
                _ => Err(matcher::Error::BadSpecialPattern(name)),
            };
        }

        default_try_match_pat(self, target, mcx)
    }
}

impl TryMatch for Ty {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        if mcx.maybe_capture_ty(self, target)? {
            return Ok(());
        }

        if let TyKind::Mac(ref mac) = self.node {
            let name = macro_name(mac);
            return match &name.as_str() as &str {
                "marked" => mcx.do_marked(&mac.node.tts,
                                          |p| p.parse_ty().map(|p| p.into_inner()),
                                          target),
                "def" => mcx.do_def_ty(&mac.node.tts, target),
                _ => Err(matcher::Error::BadSpecialPattern(name)),
            };
        }

        default_try_match_ty(self, target, mcx)
    }
}

impl TryMatch for Stmt {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        if mcx.maybe_capture_stmt(self, target)? {
            return Ok(());
        }

        default_try_match_stmt(self, target, mcx)
    }
}

impl TryMatch for Block {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        mcx.try_match(&self.id, &target.id)?;
        mcx.try_match(&self.rules, &target.rules)?;
        mcx.try_match(&self.span, &target.span)?;

        if let Some(consumed) = matcher::match_multi_stmt(mcx, &self.stmts, &target.stmts) {
            if consumed == target.stmts.len() {
                return Ok(());
            }
        }
        Err(matcher::Error::LengthMismatch)
    }
}


impl<T: TryMatch> TryMatch for [T] {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        if self.len() != target.len() {
            return Err(matcher::Error::LengthMismatch);
        }
        for i in 0 .. self.len() {
            mcx.try_match(&self[i], &target[i])?;
        }
        Ok(())
    }
}

impl<T: TryMatch> TryMatch for Vec<T> {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        <[T] as TryMatch>::try_match(self, target, mcx)
    }
}

impl<T: TryMatch> TryMatch for ThinVec<T> {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        <[T] as TryMatch>::try_match(self, target, mcx)
    }
}

impl<T: TryMatch> TryMatch for P<T> {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        mcx.try_match(&**self, &**target)
    }
}

impl<T: TryMatch> TryMatch for Rc<T> {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        mcx.try_match(&**self, &**target)
    }
}

impl<T: TryMatch> TryMatch for Spanned<T> {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        mcx.try_match(&self.node, &target.node)
    }
}

impl<T: TryMatch> TryMatch for Option<T> {
    fn try_match(&self, target: &Option<T>, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        match (self, target) {
            (&Some(ref x), &Some(ref y)) => mcx.try_match(x, y),
            (&None, &None) => Ok(()),
            (_, _) => Err(matcher::Error::VariantMismatch),
        }
    }
}

impl<A: TryMatch, B: TryMatch> TryMatch for (A, B) {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        mcx.try_match(&self.0, &target.0)?;
        mcx.try_match(&self.1, &target.1)?;
        Ok(())
    }
}

impl<A: TryMatch, B: TryMatch, C: TryMatch> TryMatch for (A, B, C) {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result<()> {
        mcx.try_match(&self.0, &target.0)?;
        mcx.try_match(&self.1, &target.1)?;
        mcx.try_match(&self.2, &target.2)?;
        Ok(())
    }
}


include!(concat!(env!("OUT_DIR"), "/matcher_impls_gen.inc.rs"));
