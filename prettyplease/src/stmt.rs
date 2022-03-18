use crate::algorithm::Printer;
use syn::{Expr, Stmt};

impl Printer {
    pub fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Local(local) => {
                self.outer_attrs(&local.attrs);
                self.ibox(0);
                self.word("let ");
                self.pat(&local.pat);
                if let Some((_eq, init)) = &local.init {
                    self.word(" = ");
                    self.neverbreak();
                    self.expr(init);
                }
                self.word(";");
                self.end();
                self.hardbreak();
            }
            Stmt::Item(item) => self.item(item),
            Stmt::Expr(expr) => {
                self.ibox(0);
                self.expr(expr);
                if add_semi(expr) {
                    self.word(";");
                }
                self.end();
                self.hardbreak();
            }
            Stmt::Semi(expr, _semi) => {
                if let Expr::Verbatim(tokens) = expr {
                    if tokens.is_empty() {
                        return;
                    }
                }
                self.ibox(0);
                self.expr(expr);
                if !remove_semi(expr) {
                    self.word(";");
                }
                self.end();
                self.hardbreak();
            }
        }
    }
}

pub fn add_semi(expr: &Expr) -> bool {
    match expr {
        Expr::Assign(_)
        | Expr::AssignOp(_)
        | Expr::Break(_)
        | Expr::Continue(_)
        | Expr::Return(_)
        | Expr::Yield(_) => true,
        Expr::Group(group) => add_semi(&group.expr),
        _ => false,
    }
}

fn remove_semi(expr: &Expr) -> bool {
    match expr {
        Expr::ForLoop(_) | Expr::While(_) => true,
        Expr::Group(group) => remove_semi(&group.expr),
        Expr::If(expr) => match &expr.else_branch {
            Some((_else_token, else_branch)) => remove_semi(else_branch),
            None => true,
        },
        _ => false,
    }
}
