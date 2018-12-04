use rustc_data_structures::sync::Lrc;
use syntax::ast::*;
use syntax::ptr::P;
use syntax::symbol::Symbol;

use api::*;
use command::{CommandState, Registry};
use driver;
use transform::Transform;


/// # `bytestr_to_str` Command
/// 
/// Usage: `bytestr_to_str`
/// 
/// Marks: `target`
/// 
/// Convert bytestring literal expressions marked `target` to string literal
/// expressions.
/// 
/// Note the mark must be placed on the expression, as it is currently difficult to
/// mark a literal node.
pub struct ByteStrToStr;

impl Transform for ByteStrToStr {
    fn transform(&self, krate: Crate, st: &CommandState, _cx: &driver::Ctxt) -> Crate {
        fold_nodes(krate, |e: P<Expr>| {
            if !st.marked(e.id, "target") {
                return e;
            }

            e.map(|e| {
                let node = match e.node {
                    ExprKind::Lit(l) => {
                        let node = match l.node {
                            LitKind::ByteStr(bs) => {
                                let s = String::from_utf8((*bs).clone()).unwrap();
                                LitKind::Str(Symbol::intern(&s), StrStyle::Cooked)
                            },
                            n => n,
                        };
                        ExprKind::Lit(Lit { node, ..l })
                    },
                    n => n,
                };
                Expr { node, ..e }
            })
        })
    }
}


/// # `remove_null_terminator` Command
/// 
/// Usage: `remove_null_terminator`
/// 
/// Marks: `target`
/// 
/// Remove a trailing `\0` character from marked string and bytestring literal
/// expressions.
/// 
/// Note the mark must be placed on the expression, as it is currently difficult to
/// mark a literal node.
pub struct RemoveNullTerminator;

impl Transform for RemoveNullTerminator {
    fn transform(&self, krate: Crate, st: &CommandState, _cx: &driver::Ctxt) -> Crate {
        fold_nodes(krate, |e: P<Expr>| {
            if !st.marked(e.id, "target") {
                return e;
            }

            e.map(|e| {
                let node = match e.node {
                    ExprKind::Lit(l) => {
                        let node = match l.node {
                            LitKind::ByteStr(bs) => {
                                if bs.last() == Some(&0) {
                                    let mut bs = (*bs).clone();
                                    bs.pop();
                                    LitKind::ByteStr(Lrc::new(bs))
                                } else {
                                    LitKind::ByteStr(bs)
                                }
                            },
                            LitKind::Str(s, style) => {
                                if s.as_str().ends_with("\0") {
                                    let end = s.as_str().len() - 1;
                                    let new_s = Symbol::intern(&s.as_str()[..end]);
                                    LitKind::Str(new_s, style)
                                } else {
                                    LitKind::Str(s, style)
                                }
                            },
                            n => n,
                        };
                        ExprKind::Lit(Lit { node, ..l })
                    },
                    n => n,
                };
                Expr { node, ..e }
            })
        })
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;
    reg.register("bytestr_to_str", |_args| mk(ByteStrToStr));
    reg.register("remove_null_terminator", |_args| mk(RemoveNullTerminator));
}

