//! This modules handles converting `Vec<Structure>` into `Vec<Stmt>`.

use super::*;

use crate::rust_ast::comment_store;

/// Convert a sequence of structures produced by Relooper back into Rust statements
pub fn structured_cfg(
    root: &Vec<Structure<StmtOrComment>>,
    comment_store: &mut comment_store::CommentStore,
    current_block: P<Expr>,
    debug_labels: bool,
    cut_out_trailing_ret: bool,
) -> Result<Vec<Stmt>, TranslationError> {
    let ast: StructuredAST<P<Expr>, P<Pat>, Label, StmtOrComment> =
        structured_cfg_help(vec![], &IndexSet::new(), root, &mut IndexSet::new())?;

    let s = StructureState {
        enable_comments: true,
        debug_labels,
        current_block,
    };
    let mut queued = vec![];
    let mut stmts = vec![];
    s.into_stmt(ast, comment_store, &mut queued, &mut stmts);
    if !queued.is_empty() {
        eprintln!("Did not find a statement for comments {:?}", queued);
    }

    // If the very last statement in the vector is a `return`, we can either cut it out or replace
    // it with the returned value.
    if cut_out_trailing_ret {
        match stmts.last().cloned() {
            Some(Stmt {
                node: StmtKind::Expr(ref ret),
                ..
            })
            | Some(Stmt {
                node: StmtKind::Semi(ref ret),
                ..
            }) => {
                match ret.node {
                    ExprKind::Ret(None) => {
                        stmts.pop();
                    }
                    // TODO: why does libsyntax print a ';' after this even if it is 'Expr' and not 'Semi'
                    //                ExprKind::Ret(Some(ref e)) => {
                    //                    stmts.pop();
                    //                    stmts.push(mk().expr_stmt(e));
                    //                }
                    _ => {}
                }
            }
            _ => {}
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
        cond: Self::E,                    // expression being matched
        cases: Vec<(Vec<Self::P>, Self)>, // match arms
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
}

/// Defunctionalized version of `StructuredStatement` trait
#[allow(missing_docs)]
pub enum StructuredAST<E, P, L, S> {
    Empty,
    Singleton(S),
    Append(
        Box<StructuredAST<E, P, L, S>>,
        Box<StructuredAST<E, P, L, S>>,
    ),
    Goto(L),
    Match(E, Vec<(Vec<P>, StructuredAST<E, P, L, S>)>),
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
        StructuredAST::Empty
    }

    fn mk_singleton(stmt: Self::S) -> Self {
        StructuredAST::Singleton(stmt)
    }

    fn mk_append(self, second: Self) -> Self {
        StructuredAST::Append(Box::new(self), Box::new(second))
    }

    fn mk_goto(to: Self::L) -> Self {
        StructuredAST::Goto(to)
    }

    fn mk_match(cond: Self::E, cases: Vec<(Vec<Self::P>, Self)>) -> Self {
        StructuredAST::Match(cond, cases)
    }

    fn mk_if(cond: Self::E, then: Self, else_: Self) -> Self {
        StructuredAST::If(cond, Box::new(then), Box::new(else_))
    }

    fn mk_goto_table(cases: Vec<(Self::L, Self)>, then: Self) -> Self {
        StructuredAST::GotoTable(cases, Box::new(then))
    }

    fn mk_loop(lbl: Option<Self::L>, body: Self) -> Self {
        StructuredAST::Loop(lbl, Box::new(body))
    }

    fn mk_exit(exit_style: ExitStyle, label: Option<Self::L>) -> Self {
        StructuredAST::Exit(exit_style, label)
    }
}

/// Recursive helper for `structured_cfg`
///
/// TODO: move this into `structured_cfg`?
fn structured_cfg_help<
    S: StructuredStatement<E = P<Expr>, P = P<Pat>, L = Label, S = StmtOrComment>,
>(
    exits: Vec<(Label, IndexMap<Label, (IndexSet<Label>, ExitStyle)>)>,
    next: &IndexSet<Label>,
    root: &Vec<Structure<StmtOrComment>>,
    used_loop_labels: &mut IndexSet<Label>,
) -> Result<S, TranslationError> {
    let mut next: &IndexSet<Label> = next;
    let mut rest: S = S::empty();

    for structure in root.iter().rev() {
        let mut new_rest: S = S::empty();

        match structure {
            &Structure::Simple {
                ref body,
                ref terminator,
                ..
            } => {
                for s in body.clone() {
                    new_rest = S::mk_append(new_rest, S::mk_singleton(s));
                }

                let insert_goto = |to: Label, target: &IndexSet<Label>| -> S {
                    if target.len() == 1 {
                        S::empty()
                    } else {
                        S::mk_goto(to)
                    }
                };

                let mut branch =
                    |slbl: &StructureLabel<StmtOrComment>| -> Result<S, TranslationError> {
                        match slbl {
                            &StructureLabel::Nested(ref nested) => {
                                structured_cfg_help(exits.clone(), next, nested, used_loop_labels)
                            }

                            &StructureLabel::GoTo(to) | &StructureLabel::ExitTo(to)
                                if next.contains(&to) =>
                            {
                                Ok(insert_goto(to, &next))
                            }

                            &StructureLabel::ExitTo(to) => {
                                let mut immediate = true;
                                for &(label, ref local) in &exits {
                                    if let Some(&(ref follow, exit_style)) = local.get(&to) {
                                        let lbl = if immediate {
                                            None
                                        } else {
                                            used_loop_labels.insert(label);
                                            Some(label)
                                        };

                                        return Ok(S::mk_append(
                                            insert_goto(to, follow),
                                            S::mk_exit(exit_style, lbl),
                                        ));
                                    }
                                    immediate = false;
                                }

                                Err(format_err!(
                                    "Not a valid exit: {:?} has nothing to exit to",
                                    to
                                )
                                .into())
                            }

                            &StructureLabel::GoTo(to) => Err(format_err!(
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
                        &End => S::empty(),
                        &Jump(ref to) => branch(to)?,
                        &Branch(ref c, ref t, ref f) => S::mk_if(c.clone(), branch(t)?, branch(f)?),
                        &Switch {
                            ref expr,
                            ref cases,
                        } => {
                            let branched_cases: Vec<(Vec<P<Pat>>, S)> = cases
                                .iter()
                                .map(|&(ref pats, ref slbl)| Ok((pats.clone(), branch(slbl)?)))
                                .collect::<Result<Vec<(Vec<P<Pat>>, S)>, TranslationError>>()?;

                            S::mk_match(expr.clone(), branched_cases)
                        }
                    },
                );
            }

            &Structure::Multiple {
                ref branches,
                ref then,
                ..
            } => {
                let cases: Vec<(Label, S)> = branches
                    .iter()
                    .map(|(lbl, body)| -> Result<(Label, S), TranslationError> {
                        let stmts =
                            structured_cfg_help(exits.clone(), next, body, used_loop_labels)?;
                        Ok((*lbl, stmts))
                    })
                    .collect::<Result<Vec<(Label, S)>, TranslationError>>()?;

                let then: S = structured_cfg_help(exits.clone(), next, then, used_loop_labels)?;

                new_rest = S::mk_append(new_rest, S::mk_goto_table(cases, then));
            }

            &Structure::Loop {
                ref body,
                ref entries,
            } => {
                let label = entries
                    .iter()
                    .next()
                    .ok_or(format_err!("The loop {:?} has no entry", structure))?;

                let mut these_exits = IndexMap::new();
                these_exits.extend(
                    entries
                        .iter()
                        .map(|e| (*e, (entries.clone(), ExitStyle::Continue))),
                );
                these_exits.extend(next.iter().map(|e| (*e, (next.clone(), ExitStyle::Break))));

                let mut exits_new = vec![(*label, these_exits)];
                exits_new.extend(exits.clone());

                let body = structured_cfg_help(exits_new, entries, body, used_loop_labels)?;
                let loop_lbl = if used_loop_labels.contains(label) {
                    Some(*label)
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
pub fn has_multiple<Stmt>(root: &Vec<Structure<Stmt>>) -> bool {
    root.iter().any(|structure| match structure {
        &Structure::Simple { ref terminator, .. } => {
            terminator
                .get_labels()
                .into_iter()
                .any(|structure_label| match structure_label {
                    &StructureLabel::Nested(ref nested) => has_multiple(nested),
                    _ => false,
                })
        }
        &Structure::Multiple { .. } => return true,
        &Structure::Loop { ref body, .. } => has_multiple(body),
    })
}

struct StructureState {
    enable_comments: bool,
    debug_labels: bool,
    current_block: P<Expr>,
}

impl StructureState {
    pub fn into_stmt(
        &self,
        ast: StructuredAST<P<Expr>, P<Pat>, Label, StmtOrComment>,
        comment_store: &mut comment_store::CommentStore,
        queued_comments: &mut Vec<String>,
        output: &mut Vec<Stmt>,
    ) {
        use crate::cfg::structures::StructuredAST::*;

        match ast {
            Empty => {}

            Singleton(StmtOrComment::Comment(c)) => {
                if self.enable_comments {
                    queued_comments.push(c);
                }
            }
            Singleton(StmtOrComment::Stmt(mut s)) => {
                s.span = comment_store.add_comment_lines(&queued_comments);
                queued_comments.clear();
                output.push(s);
            }

            Append(lhs, rhs) => {
                self.into_stmt(*lhs, comment_store, queued_comments, output);
                self.into_stmt(*rhs, comment_store, queued_comments, output);
            }

            Goto(to) => {
                // Assign to `current_block` the next label we want to go to.

                let s = comment_store.add_comment_lines(&queued_comments);
                queued_comments.clear();

                let lbl_expr = if self.debug_labels {
                    to.to_string_expr()
                } else {
                    to.to_num_expr()
                };
                output.push(
                    mk().span(s)
                        .semi_stmt(mk().assign_expr(self.current_block.clone(), lbl_expr)),
                );
            }

            Match(cond, cases) => {
                // Make a `match`.

                let s = comment_store.add_comment_lines(&queued_comments);
                queued_comments.clear();

                let arms: Vec<Arm> = cases
                    .into_iter()
                    .map(|(pats, stmts)| -> Arm {
                        let stmts = {
                            let mut output = vec![];
                            self.into_stmt(stmts, comment_store, queued_comments, &mut output);
                            output
                        };

                        let body = mk().block_expr(mk().block(stmts));
                        mk().arm(pats, None as Option<P<Expr>>, body)
                    })
                    .collect();

                let e = mk().match_expr(cond, arms);

                output.push(mk().span(s).expr_stmt(e));
            }

            If(cond, then, els) => {
                // Construct a Rust `if` statement from a condition and then/else branches
                //
                //   * `if <cond-expr> { } else { }` turns into `<cond-expr>;`
                //   * `if <cond-expr> { .. } else { }` turns into `if <cond-expr> { .. }`
                //   * `if <cond-expr> { } else { .. }` turns into `if !<cond-expr> { .. }`
                //

                let s = comment_store.add_comment_lines(&queued_comments);
                queued_comments.clear();

                let then = {
                    let mut output = vec![];
                    self.into_stmt(*then, comment_store, queued_comments, &mut output);
                    output
                };

                let mut els = {
                    let mut output = vec![];
                    self.into_stmt(*els, comment_store, queued_comments, &mut output);
                    output
                };

                let mut if_stmt = match (then.is_empty(), els.is_empty()) {
                    (true, true) => mk().semi_stmt(cond),
                    (false, true) => {
                        let if_expr =
                            mk().ifte_expr(cond, mk().block(then), None as Option<P<Expr>>);
                        mk().expr_stmt(if_expr)
                    }
                    (true, false) => {
                        let negated_cond = not(&cond);
                        let if_expr =
                            mk().ifte_expr(negated_cond, mk().block(els), None as Option<P<Expr>>);
                        mk().expr_stmt(if_expr)
                    }
                    (false, false) => {
                        fn is_expr(kind: &StmtKind) -> bool {
                            match kind {
                                &StmtKind::Expr(_) => true,
                                _ => false,
                            }
                        }

                        let is_els_expr = els.len() == 1 && is_expr(&els[0].node);

                        let els_branch = if is_els_expr {
                            match els.swap_remove(0).node {
                                StmtKind::Expr(e) => e,
                                _ => panic!("is_els_expr out of sync"),
                            }
                        } else {
                            mk().block_expr(mk().block(els))
                        };

                        let if_expr = mk().ifte_expr(cond, mk().block(then), Some(els_branch));
                        mk().expr_stmt(if_expr)
                    }
                };

                if_stmt.span = s;
                output.push(if_stmt);
            }

            GotoTable(cases, then) => {
                // Dispatch based on the next `current_block` value.

                let s = comment_store.add_comment_lines(&queued_comments);
                queued_comments.clear();

                let mut arms: Vec<Arm> = cases
                    .into_iter()
                    .map(|(lbl, stmts)| -> Arm {
                        let stmts = {
                            let mut output = vec![];
                            self.into_stmt(stmts, comment_store, queued_comments, &mut output);
                            output
                        };

                        let lbl_expr = if self.debug_labels {
                            lbl.to_string_expr()
                        } else {
                            lbl.to_num_expr()
                        };
                        let pat = mk().lit_pat(lbl_expr);
                        let body = mk().block_expr(mk().block(stmts));
                        mk().arm(vec![pat], None as Option<P<Expr>>, body)
                    })
                    .collect();

                let then = {
                    let mut output = vec![];
                    self.into_stmt(*then, comment_store, queued_comments, &mut output);
                    output
                };

                arms.push(mk().arm(
                    vec![mk().wild_pat()],
                    None as Option<P<Expr>>,
                    mk().block_expr(mk().block(then)),
                ));

                let e = mk().match_expr(self.current_block.clone(), arms);

                output.push(mk().span(s).expr_stmt(e));
            }

            Loop(lbl, body) => {
                // Make (possibly labelled) `loop`.
                //
                //   * Loops that start with an `if <cond-expr> { break; }` get converted into `while` loops
                //

                let s = comment_store.add_comment_lines(&queued_comments);
                queued_comments.clear();

                let body = {
                    let mut output = vec![];
                    self.into_stmt(*body, comment_store, queued_comments, &mut output);
                    output
                };

                // TODO: this is ugly but it needn't be. We are just pattern matching on particular ASTs.
                if let Some(&Stmt {
                    node: syntax::ast::StmtKind::Expr(ref expr),
                    ..
                }) = body.iter().nth(0)
                {
                    if let syntax::ast::ExprKind::If(ref cond, ref thn, None) = expr.node {
                        if let &syntax::ast::Block {
                            ref stmts,
                            rules: syntax::ast::BlockCheckMode::Default,
                            ..
                        } = thn.deref()
                        {
                            if stmts.len() == 1 {
                                if let Some(&Stmt {
                                    node: syntax::ast::StmtKind::Semi(ref expr),
                                    ..
                                }) = stmts.iter().nth(0)
                                {
                                    if let syntax::ast::ExprKind::Break(None, None) = expr.node {
                                        let e = mk().while_expr(
                                            not(cond),
                                            mk().block(body.iter().skip(1).cloned().collect()),
                                            lbl.map(|l| l.pretty_print()),
                                        );
                                        output.push(mk().span(s).expr_stmt(e));
                                        return;
                                    }
                                }
                            }
                        }
                    }
                }

                let e = mk().loop_expr(mk().block(body), lbl.map(|l| l.pretty_print()));

                output.push(mk().span(s).expr_stmt(e));
            }

            Exit(exit_style, lbl) => {
                // Make a (possibly labelled) `break` or `continue`.

                let s = comment_store.add_comment_lines(&queued_comments);
                queued_comments.clear();

                let lbl = lbl.map(|l| l.pretty_print());
                let e = match exit_style {
                    ExitStyle::Break => mk().break_expr(lbl),
                    ExitStyle::Continue => mk().continue_expr(lbl),
                };

                output.push(mk().span(s).semi_stmt(e));
            }
        }
    }
}

/// Take the logical negation of an expression.
///
///   * Negating something of the form `!<expr>` produces `<expr>`
///
fn not(bool_expr: &P<Expr>) -> P<Expr> {
    match bool_expr.node {
        ExprKind::Unary(syntax::ast::UnOp::Not, ref e) => e.clone(),
        _ => mk().unary_expr("!", bool_expr.clone()),
    }
}
