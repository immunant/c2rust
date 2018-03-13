//! This modules handles converting `Vec<Structure>` into `Vec<Stmt>`.

use super::*;

/// Convert a sequence of structures produced by Relooper back into Rust statements
pub fn structured_cfg(
    root: &Vec<Structure<Stmt>>,
    current_block: P<Expr>,
    debug_labels: bool
) -> Vec<Stmt> {
    let mut stmts = structured_cfg_help(
        vec![],
        &HashSet::new(),
        root,
        &mut HashSet::new(),
        current_block,
        debug_labels,
    );

    // If the very last statement in the vector is a `return`, we can either cut it out or replace
    // it with the returned value.
    match stmts.last().cloned() {
        Some(Stmt { node: StmtKind::Expr(ref ret), .. }) |
        Some(Stmt { node: StmtKind::Semi(ref ret), .. }) => {
            match ret.node {
                ExprKind::Ret(None) => {
                    stmts.pop();
                }
                // TODO: why does libsyntax print a ';' after this even if it is 'Expr' and not 'Semi'
//                ExprKind::Ret(Some(ref e)) => {
//                    stmts.pop();
//                    stmts.push(mk().expr_stmt(e));
//                }
                _ => { }
            }
        }
        _ => { }
    }

    stmts
}

/// Recursive helper for `structured_cfg`
///
/// TODO: move this into `structured_cfg`?
fn structured_cfg_help(
    exits: Vec<(Label, HashMap<Label, (HashSet<Label>, ExitStyle)>)>,
    next: &HashSet<Label>,
    root: &Vec<Structure<Stmt>>,
    used_loop_labels: &mut HashSet<Label>,
    current_block: P<Expr>,
    debug_labels: bool,
) -> Vec<Stmt> {

    let mut next: &HashSet<Label> = next;
    let mut rest: Vec<Stmt> = vec![];

    for structure in root.iter().rev() {
        let mut new_rest: Vec<Stmt> = vec![];

        match structure {
            &Structure::Simple { ref body, ref terminator, .. } => {
                new_rest.extend(body.clone());

                let insert_goto = |to: Label, target: &HashSet<Label>, stmts: Vec<Stmt>| -> Vec<Stmt> {
                    if target.len() == 1 {
                        stmts
                    } else {
                        let mut result = mk_goto(to, current_block.clone(), debug_labels);
                        result.extend(stmts);
                        result
                    }
                };

                let mut branch = |slbl: &StructureLabel<Stmt>| -> Vec<Stmt> {
                    match slbl {
                        &StructureLabel::Nested(ref nested) =>
                            structured_cfg_help(
                                exits.clone(),
                                next, nested,
                                used_loop_labels,
                                current_block.clone(),
                                debug_labels,
                            ),

                        &StructureLabel::GoTo(to) |
                        &StructureLabel::ExitTo(to) if next.contains(&to) =>
                            insert_goto(to, &next, vec![]),

                        &StructureLabel::ExitTo(to) => {

                            let mut immediate = true;
                            for &(label, ref local) in &exits {
                                if let Some(&(ref follow, exit_style)) = local.get(&to) {
                                    return insert_goto(
                                        to,
                                        follow,
                                        mk_exit(immediate, exit_style, label, used_loop_labels),
                                    )
                                }
                                immediate = false;
                            }

                            panic!("Not a valid exit - nothing to exit to")
                        }
                        _ => panic!("Not a valid exit"),
                    }
                };

                new_rest.extend(match terminator {
                    &End => vec![],
                    &Jump(ref to) => branch(to),
                    &Branch(ref c, ref t, ref f) => mk_if(c.clone(), branch(t), branch(f)),
                    &Switch { ref expr, ref cases } => {
                        let branched_cases = cases
                            .iter()
                            .map(|&(ref pats, ref slbl)| (pats.clone(), branch(slbl)))
                            .collect();

                        mk_match(expr.clone(), branched_cases)
                    },
                });
            }

            &Structure::Multiple { ref branches, ref then, .. } => {
                let cases: Vec<(Label, Vec<Stmt>)> = branches
                    .iter()
                    .map(|(lbl, body)| {
                        let stmts = structured_cfg_help(
                            exits.clone(),
                            next,
                            body,
                            used_loop_labels,
                            current_block.clone(),
                            debug_labels,
                        );
                        (*lbl, stmts)
                    })
                    .collect();

                let then: Vec<Stmt> = structured_cfg_help(
                    exits.clone(),
                    next,
                    then,
                    used_loop_labels,
                    current_block.clone(),
                    debug_labels,
                );

                new_rest.extend(mk_goto_table(cases, then, current_block.clone(), debug_labels));
            }

            &Structure::Loop { ref body, ref entries } => {
                let label = entries.iter().next().expect("there were no labels");

                let mut these_exits = HashMap::new();
                these_exits.extend(entries
                    .iter()
                    .map(|e| (*e, (entries.clone(), ExitStyle::Continue)))
                );
                these_exits.extend(next
                    .iter()
                    .map(|e| (*e, (next.clone(), ExitStyle::Break)))
                );

                let mut exits_new = vec![(*label, these_exits)];
                exits_new.extend(exits.clone());

                let body = structured_cfg_help(
                    exits_new,
                    entries,
                    body,
                    used_loop_labels,
                    current_block.clone(),
                    debug_labels,
                );
                let loop_lbl = if used_loop_labels.contains(label) { Some(*label) } else { None };
                new_rest.extend(mk_loop(loop_lbl, body));
            }
        }

        new_rest.extend(rest);

        rest = new_rest;
        next = structure.get_entries();
    }

    rest
}

/// Checks if there are any `Multiple` structures anywhere. Only if so will there be any need for a
/// `current_block` variable.
pub fn has_multiple<Stmt>(root: &Vec<Structure<Stmt>>) -> bool {
    root.iter().any(|structure| {
        match structure {
            &Structure::Simple { ref terminator, .. } => terminator
                .get_labels()
                .into_iter()
                .any(|structure_label|
                    match structure_label {
                        &StructureLabel::Nested(ref nested) => has_multiple(nested),
                        _ => false,
                    }
                ),
            &Structure::Multiple { .. } => return true,
            &Structure::Loop { ref body, .. } => has_multiple(body),
        }
    })
}


/// Construct a Rust `if` statement from a condition and then/else branches
///
///   * `if <cond-expr> { } else { }` turns into `<cond-expr>;`
///   * `if <cond-expr> { .. } else { }` turns into `if <cond-expr> { .. }`
///   * `if <cond-expr> { } else { .. }` turns into `if !<cond-expr> { .. }`
///
fn mk_if(cond: P<Expr>, then: Vec<Stmt>, mut els: Vec<Stmt>) -> Vec<Stmt> {
    let s = match (then.is_empty(), els.is_empty()) {
        (true, true) => mk().semi_stmt(cond),
        (false, true) => {
            let if_expr = mk().ifte_expr(cond, mk().block(then), None as Option<P<Expr>>);
            mk().expr_stmt(if_expr)
        },
        (true, false) => {
            let negated_cond = not(&cond);
            let if_expr = mk().ifte_expr(negated_cond, mk().block(els), None as Option<P<Expr>>);
            mk().expr_stmt(if_expr)
        },
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

    vec![s]
}

/// Make a `match`.
fn mk_match(cond: P<Expr>, cases: Vec<(Vec<P<Pat>>, Vec<Stmt>)>) -> Vec<Stmt> {

    let arms: Vec<Arm> = cases
        .into_iter()
        .map(|(pats, stmts)| -> Arm {
            let body = mk().block_expr(mk().block(stmts));
            mk().arm(pats, None as Option<P<Expr>>, body)
        })
        .collect();

    let e = mk().match_expr(cond, arms);

    vec![mk().expr_stmt(e)]
}

/// Assign to `current_block` the next label we want to go to.
fn mk_goto(to: Label, current_block: P<Expr>, debug_labels: bool) -> Vec<Stmt> {
    let lbl_expr = if debug_labels { to.to_string_expr() } else { to.to_num_expr() };
    let assign = mk().semi_stmt(mk().assign_expr(current_block, lbl_expr));
    vec![assign]
}

/// Dispatch based on the next `current_block` value.
fn mk_goto_table(
    cases: Vec<(Label, Vec<Stmt>)>,  // entries in the goto table
    then: Vec<Stmt>,                 // default case of the goto table
    current_block: P<Expr>,          // the `current_block` variable
    debug_labels: bool
) -> Vec<Stmt> {
    let mut arms: Vec<Arm> = cases
        .into_iter()
        .map(|(lbl, stmts)| -> Arm {
            let lbl_expr = if debug_labels { lbl.to_string_expr() } else { lbl.to_num_expr() };
            let pat = mk().lit_pat(lbl_expr);
            let body = mk().block_expr(mk().block(stmts));
            mk().arm(vec![pat], None as Option<P<Expr>>, body)
        })
        .collect();

    arms.push(mk().arm(
        vec![mk().wild_pat()],
        None as Option<P<Expr>>,
        mk().block_expr(mk().block(then))
    ));

    let e = mk().match_expr(current_block, arms);

    vec![mk().expr_stmt(e)]
}


/// Make (possibly labelled) `loop`.
///
///   * Loops that start with an `if <cond-expr> { break; }` get converted into `while` loops
///
fn mk_loop(lbl: Option<Label>, body: Vec<Stmt>) -> Vec<Stmt> {

    // TODO: this is ugly but it needn't be. We are just pattern matching on particular ASTs.
    if let Some(&Stmt{ node: syntax::ast::StmtKind::Expr(ref expr), .. }) = body.iter().nth(0) {
        if let syntax::ast::ExprKind::If(ref cond, ref thn, None) = expr.node {
            if let &syntax::ast::Block { ref stmts, rules: syntax::ast::BlockCheckMode::Default, .. } = thn.deref() {
                if stmts.len() == 1 {
                    if let Some(&Stmt{ node: syntax::ast::StmtKind::Semi(ref expr), .. }) = stmts.iter().nth(0) {
                        if let syntax::ast::ExprKind::Break(None, None) = expr.node {
                            let e = mk().while_expr(
                                not(cond),
                                mk().block(body.iter().skip(1).cloned().collect()),
                                lbl.map(|l| l.pretty_print()),
                            );
                            return vec![mk().expr_stmt(e)];
                        }
                    }
                }
            }
        }
    }

    let e = mk().loop_expr(mk().block(body), lbl.map(|l| l.pretty_print()));
    vec![mk().expr_stmt(e)]
}

/// Make a (possibly labelled) `break` or `continue`.
fn mk_exit(
    immediate: bool,
    exit_style: ExitStyle,
    label: Label,                          // used solely to generate a helpful loop label
    used_loop_labels: &mut HashSet<Label>  // records which loops need labels
) -> Vec<Stmt> {
    let lbl = if immediate {
        None
    } else {
        used_loop_labels.insert(label);
        Some(label.pretty_print())
    };
    let e = match exit_style {
        ExitStyle::Break => mk().break_expr(lbl),
        ExitStyle::Continue => mk().continue_expr(lbl),
    };
    vec![mk().semi_stmt(e)]
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
