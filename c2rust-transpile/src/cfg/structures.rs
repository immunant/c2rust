//! This modules handles converting `Vec<Structure>` into `Vec<Stmt>`.

use super::*;
use log::warn;
use syn::{spanned::Spanned as _, ExprBreak, ExprIf, ExprReturn, ExprUnary, Stmt};

use crate::rust_ast::{comment_store, set_span::SetSpan, BytePos, SpanExt};

/// Convert a sequence of structures produced by Relooper back into Rust statements
pub fn structured_cfg(
    root: &[Structure<Stmt>],
    comment_store: &mut comment_store::CommentStore,
    current_block: Box<Expr>,
    debug_labels: bool,
    cut_out_trailing_ret: bool,
) -> TranslationResult<Vec<Stmt>> {
    let ast: StructuredAST<Box<Expr>, Pat, Label, Stmt> =
        structured_cfg_help(vec![], &IndexSet::new(), root, &mut IndexSet::new())?;

    let s = StructureState {
        debug_labels,
        current_block,
    };
    let (mut stmts, _span) = s.to_stmt(ast, comment_store);

    // If the very last statement in the vector is a `return`, we can either cut it out or replace
    // it with the returned value.
    if cut_out_trailing_ret {
        if let Some(Stmt::Expr(Expr::Return(ExprReturn { expr: None, .. }), _)) = stmts.last() {
            stmts.pop();
        }
    }

    Ok(stmts)
}

/// Ways of exiting from a loop body
#[derive(Copy, Clone, Debug)]
pub enum ExitStyle {
    /// Jumps to the beginning of the loop body
    Continue,

    /// Jumps to the end of the loop body
    Break,
}

/// This is precisely what we need to construct structured statements
pub trait StructuredStatement: Sized {
    /// An expression
    type E;

    /// A pattern
    type P;

    /// A label
    type L;

    /// An unstructured regular statement
    type S;

    /// An empty statement
    fn empty() -> Self;

    /// Project a single statement into a structured statement
    fn mk_singleton(stmt: Self::S) -> Self;

    /// Execute one statement, then the other
    fn mk_append(self, second: Self) -> Self;

    /// Jump to a label
    fn mk_goto(to: Self::L) -> Self;

    /// Make a `match` statement
    fn mk_match(
        cond: Self::E,               // expression being matched
        cases: Vec<(Self::P, Self)>, // match arms
    ) -> Self;

    /// Make an `if` statement
    fn mk_if(cond: Self::E, then: Self, else_: Self) -> Self;

    /// Make a `goto` table
    fn mk_goto_table(
        cases: Vec<(Self::L, Self)>, // entries in the goto table
        then: Self,                  // default case of the goto table
    ) -> Self;

    /// Make some sort of loop
    fn mk_loop(lbl: Option<Self::L>, body: Self) -> Self;

    /// Make an exit from a loop
    fn mk_exit(
        exit_style: ExitStyle,  // `break` or a `continue`
        label: Option<Self::L>, // which loop are we breaking
    ) -> Self;

    fn extend_span(&mut self, span: Span);
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub type StructuredAST<E, P, L, S> = Spanned<StructuredASTKind<E, P, L, S>>;

fn dummy_spanned<T>(inner: T) -> Spanned<T> {
    Spanned {
        node: inner,
        span: Span::dummy(),
    }
}

/// Defunctionalized version of `StructuredStatement` trait
#[allow(missing_docs)]
#[derive(Debug)]
pub enum StructuredASTKind<E, P, L, S> {
    Empty,
    Singleton(S),
    Append(
        Box<StructuredAST<E, P, L, S>>,
        Box<StructuredAST<E, P, L, S>>,
    ),
    Goto(L),
    Match(E, Vec<(P, StructuredAST<E, P, L, S>)>),
    If(
        E,
        Box<StructuredAST<E, P, L, S>>,
        Box<StructuredAST<E, P, L, S>>,
    ),
    GotoTable(
        Vec<(L, StructuredAST<E, P, L, S>)>,
        Box<StructuredAST<E, P, L, S>>,
    ),
    Loop(Option<L>, Box<StructuredAST<E, P, L, S>>),
    Exit(ExitStyle, Option<L>),
}

impl<E, P, L, S> StructuredStatement for StructuredAST<E, P, L, S> {
    type E = E;
    type P = P;
    type L = L;
    type S = S;

    fn empty() -> Self {
        dummy_spanned(StructuredASTKind::Empty)
    }

    fn mk_singleton(stmt: Self::S) -> Self {
        dummy_spanned(StructuredASTKind::Singleton(stmt))
    }

    fn mk_append(self, second: Self) -> Self {
        dummy_spanned(StructuredASTKind::Append(Box::new(self), Box::new(second)))
    }

    fn mk_goto(to: Self::L) -> Self {
        dummy_spanned(StructuredASTKind::Goto(to))
    }

    fn mk_match(cond: Self::E, cases: Vec<(Self::P, Self)>) -> Self {
        dummy_spanned(StructuredASTKind::Match(cond, cases))
    }

    fn mk_if(cond: Self::E, then: Self, else_: Self) -> Self {
        dummy_spanned(StructuredASTKind::If(cond, Box::new(then), Box::new(else_)))
    }

    fn mk_goto_table(cases: Vec<(Self::L, Self)>, then: Self) -> Self {
        dummy_spanned(StructuredASTKind::GotoTable(cases, Box::new(then)))
    }

    fn mk_loop(lbl: Option<Self::L>, body: Self) -> Self {
        dummy_spanned(StructuredASTKind::Loop(lbl, Box::new(body)))
    }

    fn mk_exit(exit_style: ExitStyle, label: Option<Self::L>) -> Self {
        dummy_spanned(StructuredASTKind::Exit(exit_style, label))
    }

    fn extend_span(&mut self, span: Span) {
        if !self.span.is_dummy() {
            self.span = span_subst_hi(self.span, span).unwrap_or_else(|| {
                warn!("Could not extend span {:?} to {:?}", self.span, span);
                self.span
            });
        } else {
            self.span = span;
        }
    }
}

type Exit = (Label, IndexMap<Label, (IndexSet<Label>, ExitStyle)>);

/// Recursive helper for `structured_cfg`
///
/// TODO: move this into `structured_cfg`?
fn structured_cfg_help<S: StructuredStatement<E = Box<Expr>, P = Pat, L = Label, S = Stmt>>(
    exits: Vec<Exit>,
    next: &IndexSet<Label>,
    root: &[Structure<Stmt>],
    used_loop_labels: &mut IndexSet<Label>,
) -> TranslationResult<S> {
    let mut next: &IndexSet<Label> = next;
    let mut rest: S = S::empty();

    for structure in root.iter().rev() {
        let mut new_rest: S = S::empty();

        use Structure::*;
        match structure {
            Simple {
                body,
                terminator,
                span,
                ..
            } => {
                for s in body.clone() {
                    new_rest = S::mk_append(new_rest, S::mk_singleton(s));
                }
                new_rest.extend_span(*span);

                let insert_goto = |to: Label, target: &IndexSet<Label>| -> S {
                    if target.len() == 1 {
                        S::empty()
                    } else {
                        S::mk_goto(to)
                    }
                };

                let mut branch = |slbl: &StructureLabel<Stmt>| -> TranslationResult<S> {
                    use StructureLabel::*;
                    match slbl {
                        Nested(ref nested) => {
                            structured_cfg_help(exits.clone(), next, nested, used_loop_labels)
                        }

                        GoTo(to) | ExitTo(to) if next.contains(to) => {
                            Ok(insert_goto(to.clone(), next))
                        }

                        ExitTo(to) => {
                            let mut immediate = true;
                            for (label, local) in &exits {
                                if let Some(&(ref follow, exit_style)) = local.get(to) {
                                    let lbl = if immediate {
                                        None
                                    } else {
                                        used_loop_labels.insert(label.clone());
                                        Some(label.clone())
                                    };

                                    let mut new_cfg = S::mk_append(
                                        insert_goto(to.clone(), follow),
                                        S::mk_exit(exit_style, lbl),
                                    );
                                    new_cfg.extend_span(*span);
                                    return Ok(new_cfg);
                                }
                                immediate = false;
                            }

                            Err(
                                format_err!("Not a valid exit: {:?} has nothing to exit to", to)
                                    .into(),
                            )
                        }

                        GoTo(to) => Err(format_err!(
                            "Not a valid exit: {:?} (GoTo isn't falling through to {:?})",
                            to,
                            next
                        )
                        .into()),
                    }
                };

                new_rest = S::mk_append(
                    new_rest,
                    match terminator {
                        End => S::empty(),
                        Jump(to) => branch(to)?,
                        Branch(c, t, f) => S::mk_if(c.clone(), branch(t)?, branch(f)?),
                        Switch { expr, cases } => {
                            let branched_cases = cases
                                .iter()
                                .map(|(pat, slbl)| Ok((pat.clone(), branch(slbl)?)))
                                .collect::<TranslationResult<_>>()?;

                            S::mk_match(expr.clone(), branched_cases)
                        }
                    },
                );
            }

            Multiple { branches, then, .. } => {
                let cases = branches
                    .iter()
                    .map(|(lbl, body)| {
                        let stmts =
                            structured_cfg_help(exits.clone(), next, body, used_loop_labels)?;
                        Ok((lbl.clone(), stmts))
                    })
                    .collect::<TranslationResult<_>>()?;

                let then: S = structured_cfg_help(exits.clone(), next, then, used_loop_labels)?;

                new_rest = S::mk_append(new_rest, S::mk_goto_table(cases, then));
            }

            Loop { body, entries } => {
                let label = entries
                    .iter()
                    .next()
                    .ok_or_else(|| format_err!("The loop {:?} has no entry", structure))?;

                let mut these_exits = IndexMap::new();
                these_exits.extend(
                    entries
                        .iter()
                        .map(|e| (e.clone(), (entries.clone(), ExitStyle::Continue))),
                );
                these_exits.extend(
                    next.iter()
                        .map(|e| (e.clone(), (next.clone(), ExitStyle::Break))),
                );

                let mut exits_new = vec![(label.clone(), these_exits)];
                exits_new.extend(exits.clone());

                let body = structured_cfg_help(exits_new, entries, body, used_loop_labels)?;
                let loop_lbl = if used_loop_labels.contains(label) {
                    Some(label.clone())
                } else {
                    None
                };
                new_rest = S::mk_append(new_rest, S::mk_loop(loop_lbl, body));
            }
        }

        new_rest = S::mk_append(new_rest, rest);

        rest = new_rest;
        next = structure.get_entries();
    }

    Ok(rest)
}

/// Checks if there are any `Multiple` structures anywhere. Only if so will there be any need for a
/// `current_block` variable.
pub fn has_multiple<Stmt>(root: &[Structure<Stmt>]) -> bool {
    use Structure::*;
    root.iter().any(|structure| match structure {
        Simple { terminator, .. } => {
            terminator
                .get_labels()
                .into_iter()
                .any(|structure_label| match structure_label {
                    StructureLabel::Nested(nested) => has_multiple(nested),
                    _ => false,
                })
        }
        Multiple { .. } => true,
        Loop { body, .. } => has_multiple(body),
    })
}

struct StructureState {
    debug_labels: bool,
    current_block: Box<Expr>,
}

/// Returns a `Span` between the beginning of `span` or `other`, whichever is
/// non-zero, and the end of `span`. If both `span` and `other` have non-zero
/// beginnings, return `None`.
fn span_subst_lo(span: Span, other: Span) -> Option<Span> {
    if span.is_dummy() {
        return Some(other.shrink_to_lo());
    } else if span.lo() == BytePos(0) {
        return Some(span.between(other));
    } else if other.lo() != BytePos(0) && other.lo() != span.lo() {
        return None;
    }
    Some(span)
}

/// Returns a `Span` between the beginning of `span` and the end of `span` or
/// `other`, whichever is non-zero. If both `span` and `other` have non-zero
/// endings, return `None`.
fn span_subst_hi(span: Span, other: Span) -> Option<Span> {
    if other.lo() != other.hi() {
        if span.lo() == span.hi() {
            return Some(other.between(span));
        } else if other.hi() != span.hi() {
            return None;
        }
    }
    Some(span)
}

impl StructureState {
    pub fn to_stmt(
        &self,
        ast: StructuredAST<Box<Expr>, Pat, Label, Stmt>,
        comment_store: &mut comment_store::CommentStore,
    ) -> (Vec<Stmt>, Span) {
        use crate::cfg::structures::StructuredASTKind::*;

        let span = ast.span;

        let stmt = match ast.node {
            Empty => return (vec![], ast.span),

            Singleton(mut s) => {
                let span = s.span().substitute_dummy(ast.span);
                s.set_span(span);
                return (vec![s], span);
            }

            Append(spanned, rhs) if matches!(spanned.node, Empty) => {
                let lhs_span = spanned.span;
                let span = ast.span.substitute_dummy(lhs_span);
                let span = span_subst_lo(span, lhs_span).unwrap_or_else(|| {
                    comment_store.move_comments(lhs_span.lo(), span.lo());
                    span
                });

                let (mut stmts, stmts_span) = self.to_stmt(*rhs, comment_store);
                let span = span_subst_hi(span, stmts_span).unwrap_or(span);

                // Adjust the first and last elements of the block if this AST
                // node has a span.
                if let Some(stmt) = stmts.first_mut() {
                    stmt.set_span(span_subst_lo(stmt.span(), span).unwrap_or_else(|| {
                        comment_store.move_comments(stmt.span().lo(), span.lo());
                        stmt.span().with_lo(span.lo())
                    }));
                }
                if let Some(stmt) = stmts.last_mut() {
                    stmt.set_span(span_subst_hi(stmt.span(), span).unwrap_or_else(|| stmt.span()));
                }
                return (stmts, span);
            }

            Append(lhs, rhs) => {
                let (mut stmts, lhs_span) = self.to_stmt(*lhs, comment_store);
                let span = ast.span.substitute_dummy(lhs_span);
                let span = span_subst_lo(span, lhs_span).unwrap_or_else(|| {
                    comment_store.move_comments(lhs_span.lo(), span.lo());
                    span
                });
                let (rhs_stmts, rhs_span) = self.to_stmt(*rhs, comment_store);
                let span = span_subst_hi(span, rhs_span).unwrap_or(span);
                stmts.extend(rhs_stmts);
                // Adjust the first and last elements of the block if this AST
                // node has a span.
                if let Some(stmt) = stmts.first_mut() {
                    stmt.set_span(span_subst_lo(stmt.span(), span).unwrap_or_else(|| {
                        comment_store.move_comments(stmt.span().lo(), span.lo());
                        stmt.span().with_lo(span.lo())
                    }));
                }
                if let Some(stmt) = stmts.last_mut() {
                    stmt.set_span(span_subst_hi(stmt.span(), span).unwrap_or_else(|| stmt.span()));
                }
                return (stmts, span);
            }

            Goto(to) => {
                // Assign to `current_block` the next label we want to go to.

                let lbl_expr = if self.debug_labels {
                    to.to_string_expr()
                } else {
                    to.to_num_expr()
                };
                mk().span(span)
                    .semi_stmt(mk().assign_expr(self.current_block.clone(), lbl_expr))
            }

            Match(cond, cases) => {
                // Make a `match`.

                let arms: Vec<Arm> = cases
                    .into_iter()
                    .map(|(pat, stmts)| -> Arm {
                        let (stmts, span) = self.to_stmt(stmts, comment_store);

                        let body = mk().block_expr(mk().span(span).block(stmts));
                        mk().arm(pat, None, body)
                    })
                    .collect();

                let e = mk().match_expr(cond, arms);

                mk().span(span).expr_stmt(e)
            }

            If(cond, then, els) => {
                // Construct a Rust `if` statement from a condition and then/else branches
                //
                //   * `if <cond-expr> { } else { }` turns into `<cond-expr>;`
                //   * `if <cond-expr> { .. } else { }` turns into `if <cond-expr> { .. }`
                //   * `if <cond-expr> { } else { .. }` turns into `if !<cond-expr> { .. }`
                //

                let (then_stmts, then_span) = self.to_stmt(*then, comment_store);

                let (mut els_stmts, els_span) = self.to_stmt(*els, comment_store);

                let mut if_stmt = match (then_stmts.is_empty(), els_stmts.is_empty()) {
                    (true, true) => mk().semi_stmt(cond),
                    (false, true) => {
                        let if_expr =
                            mk().ifte_expr(cond, mk().span(then_span).block(then_stmts), None);
                        mk().expr_stmt(if_expr)
                    }
                    (true, false) => {
                        let negated_cond = not(&cond);
                        let if_expr = mk().ifte_expr(
                            negated_cond,
                            mk().span(els_span).block(els_stmts),
                            None,
                        );
                        mk().expr_stmt(if_expr)
                    }
                    (false, false) => {
                        fn is_expr(kind: &Stmt) -> bool {
                            matches!(kind, Stmt::Expr(Expr::If(..) | Expr::Block(..), None))
                        }

                        // Do the else statements contain a single If, IfLet or
                        // Block expression? The pretty printer handles only
                        // these kinds of expressions for the else case.
                        let is_els_expr = els_stmts.len() == 1 && is_expr(&els_stmts[0]);

                        let els_branch = if is_els_expr {
                            let stmt_expr = els_stmts.swap_remove(0);
                            let stmt_expr_span = stmt_expr.span();
                            let mut els_expr = match stmt_expr {
                                Stmt::Expr(e, None) => e,
                                _ => panic!("is_els_expr out of sync"),
                            };
                            els_expr.set_span(stmt_expr_span);
                            Box::new(els_expr)
                        } else {
                            mk().block_expr(mk().span(els_span).block(els_stmts))
                        };

                        let if_expr = mk().ifte_expr(
                            cond,
                            mk().span(then_span).block(then_stmts),
                            Some(els_branch),
                        );
                        mk().expr_stmt(if_expr)
                    }
                };

                if_stmt.set_span(span);
                if_stmt
            }

            GotoTable(cases, then) => {
                // Dispatch based on the next `current_block` value.

                let mut arms: Vec<Arm> = cases
                    .into_iter()
                    .map(|(lbl, stmts)| -> Arm {
                        let (stmts, stmts_span) = self.to_stmt(stmts, comment_store);

                        let lbl_lit = if self.debug_labels {
                            lbl.to_string_lit()
                        } else {
                            lbl.to_int_lit()
                        };
                        let pat = mk().lit_pat(lbl_lit);
                        let body = mk().block_expr(mk().span(stmts_span).block(stmts));
                        mk().arm(pat, None, body)
                    })
                    .collect();

                let (then, then_span) = self.to_stmt(*then, comment_store);

                arms.push(mk().arm(
                    mk().wild_pat(),
                    None,
                    mk().block_expr(mk().span(then_span).block(then)),
                ));

                let e = mk().match_expr(self.current_block.clone(), arms);

                mk().span(span).expr_stmt(e)
            }

            Loop(lbl, body) => {
                // Make (possibly labelled) `loop`.
                //
                //   * Loops that start with an `if <cond-expr> { break; }` get converted into `while` loops
                //

                let (body, body_span) = self.to_stmt(*body, comment_store);

                // TODO: this is ugly but it needn't be. We are just pattern matching on particular ASTs.
                if let Some(stmt @ &Stmt::Expr(ref expr, None)) = body.first() {
                    let stmt_span = stmt.span();
                    let span = if !stmt_span.is_dummy() {
                        stmt_span
                    } else {
                        span
                    };
                    if let syn::Expr::If(ExprIf {
                        cond,
                        then_branch,
                        else_branch: None,
                        ..
                    }) = expr
                    {
                        if let [Stmt::Expr(
                            syn::Expr::Break(ExprBreak {
                                label: None,
                                expr: None,
                                ..
                            }),
                            Some(_),
                        )] = then_branch.stmts.as_slice()
                        {
                            let e = mk().while_expr(
                                not(cond),
                                mk().span(body_span)
                                    .block(body.iter().skip(1).cloned().collect()),
                                lbl.map(|l| l.pretty_print()),
                            );
                            return (vec![mk().span(span).expr_stmt(e)], ast.span);
                        }
                    }
                }

                let e = mk().loop_expr(
                    mk().span(body_span).block(body),
                    lbl.map(|l| l.pretty_print()),
                );

                mk().span(span).expr_stmt(e)
            }

            Exit(exit_style, lbl) => {
                // Make a (possibly labelled) `break` or `continue`.

                let lbl = lbl.map(|l| l.pretty_print());
                let e = match exit_style {
                    ExitStyle::Break => mk().break_expr(lbl),
                    ExitStyle::Continue => mk().continue_expr(lbl),
                };

                mk().span(span).semi_stmt(e)
            }
        };

        (vec![stmt], ast.span)
    }
}

/// Take the logical negation of an expression.
///
///   * Negating something of the form `!<expr>` produces `<expr>`
///
fn not(bool_expr: &Expr) -> Box<Expr> {
    use syn::UnOp;
    match *bool_expr {
        Expr::Unary(ExprUnary {
            op: UnOp::Not(_),
            ref expr,
            ..
        }) => Box::new(unparen(expr).clone()),
        _ => mk().unary_expr(UnOp::Not(Default::default()), Box::new(bool_expr.clone())),
    }
}
