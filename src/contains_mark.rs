use syntax::ast::*;
use syntax::codemap::Span;
use syntax::ptr::P;
use syntax::parse::token::{Token, Nonterminal};
use syntax::symbol::Symbol;
use syntax::tokenstream::{TokenTree, TokenStream};
use syntax::util::small_vector::SmallVector;
use syntax::visit::{self, Visitor};

use command::CommandState;
use util::IntoSymbol;
use visit::Visit;



struct ContainsMarkVisitor<'a> {
    st: &'a CommandState,
    label: Symbol,
    found: bool,
}

macro_rules! gen_method {
    ($name:ident (& $lt:tt $ArgTy:ty) -> $walk:ident) => {
        fn $name(&mut self, x: & $lt $ArgTy) {
            if self.found {
                return;
            }

            if self.st.marked(x.id, self.label) {
                self.found = true;
                return;
            }

            visit::$walk(self, x);
        }
    };
}

impl<'a, 'ast> Visitor<'ast> for ContainsMarkVisitor<'a> {
    gen_method!(visit_expr(&'ast Expr) -> walk_expr);
    gen_method!(visit_pat(&'ast Pat) -> walk_pat);
    gen_method!(visit_ty(&'ast Ty) -> walk_ty);
    gen_method!(visit_stmt(&'ast Stmt) -> walk_stmt);
    gen_method!(visit_item(&'ast Item) -> walk_item);
}

pub fn contains_mark<T, S>(target: &T, label: S, st: &CommandState) -> bool
        where T: Visit, S: IntoSymbol {
    let mut v = ContainsMarkVisitor {
        st: st,
        label: label.into_symbol(),
        found: false,
    };
    target.visit(&mut v);
    v.found
}
