use std::collections::hash_map::{HashMap, Entry};
use std::fmt::Debug;
use std::result;
use syntax::ast::{Ident, Expr, ExprKind, Stmt, Item, Crate, Mac};
use syntax::symbol::Symbol;
use syntax::ptr::P;
use syntax::visit::{self, Visitor};

pub type Result = result::Result<(), Error>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Error {
    NotImplemented,
    VariantMismatch,
    LengthMismatch,
    SymbolMismatch,

    NotCapturing,

    // For nonlinear patterns, it's possible that the 2nd+ occurrence of the variable in the
    // pattern matches a different ident/expr/stmt than the 1st occurrence.
    NonlinearMismatch,
}

#[derive(Debug)]
pub struct MatchCtxt {
    pub cap_idents: HashMap<Symbol, P<Ident>>,
    pub cap_exprs: HashMap<Symbol, P<Expr>>,
}

impl MatchCtxt {
    pub fn new() -> MatchCtxt {
        MatchCtxt {
            cap_idents: HashMap::new(),
            cap_exprs: HashMap::new(),
        }
    }

    pub fn from_match<T: TryMatch>(pat: &T, target: &T) -> result::Result<MatchCtxt, Error> {
        let mut m = MatchCtxt::new();
        m.try_match(pat, target)?;
        Ok(m)
    }

    pub fn try_match<T: TryMatch>(&mut self, pat: &T, target: &T) -> Result {
        let r = pat.try_match(target, self);
        println!("match {:?} <- {:?}? {:?}", pat, target, r);
        r
    }

    fn try_capture<T>(cap_nodes: &mut HashMap<Symbol, P<T>>, sym: Symbol, node: &T) -> Result
            where T: Clone+Eq+'static {
        match cap_nodes.entry(sym) {
            Entry::Vacant(e) => {
                e.insert(P(node.clone()));
                Ok(())
            },
            Entry::Occupied(e) => {
                if &**e.get() != node {
                    Err(Error::NonlinearMismatch)
                } else {
                    Ok(())
                }
            },
        }
    }

    pub fn try_capture_ident(&mut self, sym: Symbol, ident: &Ident) -> Result {
        Self::try_capture(&mut self.cap_idents, sym, ident)
    }

    pub fn expr_capture_sym(pat: &Expr) -> Option<Symbol> {
        if let ExprKind::Path(None, ref path) = pat.node {
            if path.segments.len() == 1 && path.segments[0].parameters.is_none() {
                return Some(path.segments[0].identifier.name);
            }
        }
        None
    }

    pub fn try_capture_expr(&mut self, pat: &Expr, expr: &Expr) -> Result {
        if let Some(sym) = Self::expr_capture_sym(pat) {
            Self::try_capture(&mut self.cap_exprs, sym, expr)
        } else {
            Err(Error::NotCapturing)
        }
    }
}

pub trait TryMatch: Debug {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> Result;
}



struct FirstExprVisitor<'p> {
    pat: &'p Expr,
    result: Option<MatchCtxt>,
}

impl<'a, 'p> Visitor<'a> for FirstExprVisitor<'p> {
    fn visit_item(&mut self, i: &'a Item) {
        if self.result.is_some() {
            return;
        }
        visit::walk_item(self, i);
    }

    fn visit_stmt(&mut self, s: &'a Stmt) {
        if self.result.is_some() {
            return;
        }
        visit::walk_stmt(self, s);
    }

    fn visit_expr(&mut self, e: &'a Expr) {
        println!("consider: {:?}", e);
        if self.result.is_some() {
            return;
        }

        if let Ok(mcx) = MatchCtxt::from_match(self.pat, e) {
            self.result = Some(mcx);
        } else {
            visit::walk_expr(self, e);
        }
    }

    fn visit_mac(&mut self, mac: &'a Mac) {
        // TODO
    }
}

pub fn match_first_expr(pat: &Expr, ast: &Crate) -> Option<MatchCtxt> {
    let mut v = FirstExprVisitor {
        pat: pat,
        result: None
    };
    visit::walk_crate(&mut v, ast);
    v.result
}

