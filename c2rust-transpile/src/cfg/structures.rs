//! This modules handles converting `Vec<Structure>` into `Vec<Stmt>`.

use super::*;
use log::warn;
use syn::{spanned::Spanned as _, ExprBreak, ExprIf, ExprReturn, ExprUnary, Stmt};

use crate::rust_ast::{comment_store, set_span::SetSpan, BytePos, SpanExt};

/// Convert a sequence of structures produced by Relooper back into Rust statements
pub fn structured_cfg(
    root: &[Structure<Stmt>],
    checked_entries: &IndexSet<Label>,
    comment_store: &mut comment_store::CommentStore,
    current_block: Box<Expr>,
    debug_labels: bool,
    cut_out_trailing_ret: bool,
) -> TranslationResult<Vec<Stmt>> {
    let mut ast = process_cfg(
        root,
        &checked_entries,
        &IndexSet::new(),
        &None,
        &mut IndexSet::new(),
    )?;

    // TODO: It would be good to be able to spit out the AST before label cleanup
    // for debugging purposes.
    cleanup_labels(&mut ast, &None, &mut IndexSet::new());

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

/// Simplifies the relooped AST by removing labels from exits and moving block
/// labels to loop labels.
///
/// This is an optimization pass on the relooped AST. When building the AST from
/// the structured CFG, it's easier and less error-prone to always label all
/// loops and exits. But doing so makes the resulting code more verbose and gets
/// in the way of our ability to generate `while` loops, which relies on `loop {
/// if cond { break; } }` being in the AST.
///
/// This function walks the AST, keeping track of when we're allowed to use
/// unlabeled exits (i.e. when we're in a loop and NOT inside a labeled block),
/// and removes labels from exits that don't need them. We then also remove
/// labels from loops if there aren't any exits that need the label.
///
/// We also simplify the structure of the AST by removing blocks that contain an
/// unlabeled loop by moving the label to the loop.
fn cleanup_labels(
    ast: &mut StructuredAST<Box<Expr>, Pat, Label, Stmt>,
    current_loop: &Option<Label>,
    encountered_labels: &mut IndexSet<Label>,
) {
    use StructuredASTKind::*;

    match &mut ast.node {
        // Remove the label if an exit targets the loop it's directly inside of. If we
        // can't remove the label, track it in `encountered_labels` so we can later
        // decide if the loop needs its label.
        Exit(_, label) => {
            if label == current_loop {
                *label = None;
            } else if let Some(label) = label {
                encountered_labels.insert(label.clone());
            }
        }

        // Recurse through loops and blocks, tracking if we're directly inside a loop
        // and cleaning up labels where possible.
        Loop(label_place, body) => {
            let mut inner_labels = IndexSet::new();
            cleanup_labels(body, label_place, &mut inner_labels);

            // Remove the loop's label if it's never used in the loop's body.
            if let Some(label) = label_place && !inner_labels.contains(label) {
                *label_place = None;
            }

            encountered_labels.extend(inner_labels);
        }
        Block(label, body) => {
            let mut inner_labels = IndexSet::new();
            cleanup_labels(body, &None, &mut inner_labels);

            match &mut body.node {
                // If, after cleaning up labels in the block's body, the block's only contents
                // are an unlabeled loop, remove the block and apply the label to the loop
                // instead. After doing this we must then re-process the block's contents to see
                // if we can remove more labels from the loop's body.
                Loop(loop_label @ None, _) => {
                    *loop_label = Some(label.clone());

                    inner_labels.clear();
                    cleanup_labels(body, &None, &mut inner_labels);

                    *ast = std::mem::replace(&mut *body, dummy_spanned(StructuredASTKind::Empty));
                }

                // If the block's body is a labeled loop or a block, then exiting either takes
                // us to the same place. In that case we can merge the two labels together and
                // then replace the block with its contents.
                //
                // NOTE: In the future we may want to be a bit smarter about which label we
                // choose to use. Currently we always replaces the inner label with the outer
                // label, but it's possible that the inner label might have a more meaningful
                // name. The names of labels are either taken from `goto` labels in the original
                // C, or are synthetic, numeric names. At time of writing, when we do generate
                // an AST like this (i.e. a block containing a labeled loop), the inner loop
                // will have a synthetic label and the outer block will have the `goto` label
                // from the original C (this comes from the fact that the original C has to use
                // a `goto` to exit nested loops).
                //
                // In theory the smarter approach is to look at both labels, and if one of them
                // is a named label then choose that one, preferring the outer label if both are
                // named. However the current approach is simpler and does the right thing for
                // now.
                Loop(Some(inner_label), _) => {
                    // Clone the label to avoid borrowing `body` while we modify it.
                    let inner_label = inner_label.clone();
                    merge_labels(body, &inner_label, label);

                    // The loop's label has changed, re-process it to see if any more labels can be
                    // removed.
                    inner_labels.clear();
                    cleanup_labels(body, &None, &mut inner_labels);

                    *ast = std::mem::replace(&mut *body, dummy_spanned(StructuredASTKind::Empty));
                }
                Block(inner_label, _) => {
                    // Clone the label to avoid borrowing `body` while we modify it.
                    let inner_label = inner_label.clone();
                    merge_labels(body, &inner_label, label);
                    *ast = std::mem::replace(&mut *body, dummy_spanned(StructuredASTKind::Empty));
                }

                _ => {}
            }

            encountered_labels.extend(inner_labels);
        }

        // Recurse through the rest of the AST.
        Append(left, right) => {
            cleanup_labels(left, current_loop, encountered_labels);
            cleanup_labels(right, current_loop, encountered_labels);
        }
        Match(_, arms) => {
            for (_, arm) in arms {
                cleanup_labels(arm, current_loop, encountered_labels);
            }
        }
        If(_, then, else_) => {
            cleanup_labels(then, current_loop, encountered_labels);
            cleanup_labels(else_, current_loop, encountered_labels);
        }
        GotoTable(cases, then) => {
            for (_, case) in cases {
                cleanup_labels(case, current_loop, encountered_labels);
            }
            cleanup_labels(then, current_loop, encountered_labels);
        }
        Empty | Singleton(_) | Goto(_) => {}
    }
}

/// Rewrites the AST to replace one label with another.
fn merge_labels(ast: &mut StructuredAST<Box<Expr>, Pat, Label, Stmt>, old: &Label, new: &Label) {
    use StructuredASTKind::*;

    match &mut ast.node {
        // Rewrite labels for exits, blocks, and loops.
        Exit(_, Some(label)) => {
            if label == old {
                *label = new.clone();
            }
        }
        Block(label, body) | Loop(Some(label), body) => {
            if label == old {
                *label = new.clone();
            }
            merge_labels(body, old, new);
        }

        // Recurse through the rest of the AST.
        Loop(None, body) => merge_labels(body, old, new),
        Append(left, right) => {
            merge_labels(left, old, new);
            merge_labels(right, old, new);
        }
        Match(_, arms) => {
            for (_, arm) in arms {
                merge_labels(arm, old, new);
            }
        }
        If(_, then, else_) => {
            merge_labels(then, old, new);
            merge_labels(else_, old, new);
        }
        GotoTable(cases, then) => {
            for (_, case) in cases {
                merge_labels(case, old, new);
            }
            merge_labels(then, old, new);
        }
        Exit(_, None) | Empty | Singleton(_) | Goto(_) => {}
    }
}

/// Ways of exiting from a loop body
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

    fn mk_block(lbl: Self::L, body: Self) -> Self;

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

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl<T: Eq> Eq for Spanned<T> {}

pub type StructuredAST<E, P, L, S> = Spanned<StructuredASTKind<E, P, L, S>>;

fn dummy_spanned<T>(inner: T) -> Spanned<T> {
    Spanned {
        node: inner,
        span: Span::dummy(),
    }
}

/// Defunctionalized version of `StructuredStatement` trait
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq)]
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
    Block(L, Box<StructuredAST<E, P, L, S>>),
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

    fn mk_block(lbl: Self::L, body: Self) -> Self {
        dummy_spanned(StructuredASTKind::Block(lbl, Box::new(body)))
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

#[allow(dead_code)]
type Exit = (Label, IndexMap<Label, (IndexSet<Label>, ExitStyle)>);

/// Searches the structured CFG for loops with multiple entries.
///
/// When building the structured AST we need to known when a branch targets a
/// one of the entries of a loop with multiple entries, since these branches
/// need a `Goto` node in the AST (i.e. we need to set `current_block` before
/// branching). This function gathers the set of labels that need `Goto` within
/// the structured CFG.
pub fn find_checked_entries(structures: &[Structure<Stmt>], checked_entries: &mut IndexSet<Label>) {
    for structure in structures {
        match structure {
            Structure::Loop { entries, body } => {
                if entries.len() > 1 {
                    checked_entries.extend(entries.iter().cloned());
                }
                find_checked_entries(body, checked_entries);
            }
            Structure::Multiple { branches, .. } => {
                for branch in branches.values() {
                    find_checked_entries(branch, checked_entries);
                }
            }
            Structure::Simple { .. } => {}
        }
    }
}

fn process_cfg<S: StructuredStatement<E = Box<Expr>, P = Pat, L = Label, S = Stmt>>(
    structures: &[Structure<Stmt>],
    checked_entries: &IndexSet<Label>, // Labels that require `current_block` to be set before traveling to them.
    followup_entries: &IndexSet<Label>, // The entries to the next structure after our parent structure.
    loop_context: &Option<(Label, &IndexSet<Label>)>, // The label for the loop we're currently inside of, along with the entries to the next structure after the loop.
    break_targets: &mut IndexSet<Label>, // Any labels that we've had to indirectly `break` to. This tells us when we need to generate blocks as break targets.
) -> TranslationResult<S> {
    use Structure::*;

    // HACK: Reorder the branches of a multiple to put all named labels at the end.
    //
    // From a CFG perspective there's no inherent order to the branches of a
    // `Multiple`, and the order we choose when building the structured CFG is
    // currently arbitrary (i.e. we don't enforce a particular ordering). However,
    // the order we visit the branches here determines when we can fall through to
    // the branch vs when we need to `break` to it: Only the first branch will be
    // arrived at naturally, and all subsequent branches will be `break` targets.
    //
    // For the common `goto error` pattern in C code, we'll end up with a `Multiple`
    // where some of the branches are named and some aren't, but they may appear in
    // any order. In this case, pushing the named label(s) to the end is a
    // reasonable heuristic because the named labels had to be `goto` targets, and
    // therefore shouldn't be the code we flow to directly.
    //
    // This only papers over one part of the larger problem of how we order CFG
    // nodes. See https://github.com/immunant/c2rust/issues/1542 for more
    // information about how we have similar ordering problems for branches of
    // adjacent `Multiple`s. Fixing that issue should also allow us to remove this
    // hack.
    fn sort_branches(branches: &IndexMap<Label, Vec<Structure<Stmt>>>) -> Vec<Label> {
        let (named, mut rest) = branches
            .keys()
            .cloned()
            .partition::<Vec<_>, _>(|lbl| matches!(lbl, Label::FromC(_, Some(_))));
        rest.extend(named);
        rest
    }

    // Gets the entries for the structure at index `i`. If we are looking past the
    // last structure, then we use `followup_entries`.
    let get_entries = |i: usize| {
        if i < structures.len() {
            match &structures[i] {
                Simple { entries, .. } => entries.clone(),
                Loop { entries, .. } => entries.clone(),
                Multiple { branches, .. } => {
                    indexset! { sort_branches(branches).first().unwrap().clone() }
                }
            }
        } else {
            followup_entries.clone()
        }
    };

    let mut ast = S::empty();
    let mut i = 0;
    while i < structures.len() {
        let structure = &structures[i];
        let next_entries = get_entries(i + 1);

        // Generate the AST for the current structure.
        let mut structure_ast = match structure {
            Simple {
                body,
                span,
                terminator,
                ..
            } => {
                let mut body_ast: S = S::empty();
                for s in body.clone() {
                    body_ast = S::mk_append(body_ast, S::mk_singleton(s));
                }
                body_ast.extend_span(*span);

                let mut branch = |slbl: &StructureLabel<Stmt>| -> TranslationResult<S> {
                    use StructureLabel::*;

                    match slbl {
                        Nested(nested) => process_cfg(
                            nested,
                            checked_entries,
                            &next_entries,
                            loop_context,
                            break_targets,
                        ),

                        BreakTo(to) => {
                            let mut new_ast = if checked_entries.contains(to) {
                                S::mk_goto(to.clone())
                            } else {
                                S::empty()
                            };

                            // Only generate an exit if we're not going to flow naturally into the
                            // next structure's entries.
                            if !next_entries.contains(to) {
                                let label = Some(to.clone());
                                break_targets.insert(to.clone());
                                new_ast =
                                    S::mk_append(new_ast, S::mk_exit(ExitStyle::Break, label));
                            }
                            new_ast.extend_span(*span);
                            Ok(new_ast)
                        }

                        ContinueTo { loop_label, target } => {
                            let mut new_ast = if checked_entries.contains(target) {
                                S::mk_goto(target.clone())
                            } else {
                                S::empty()
                            };

                            // If we can get where we're going with a `break`, prefer that over
                            // using a `continue`. This helps us to reconstruct `while` loops in
                            // more cases, since generating a `while` requires the AST to be
                            // structured like `loop { if { break; } }`.
                            if let Some((current_loop, loop_exits)) = loop_context && loop_exits.contains(loop_label) {
                                new_ast = S::mk_append(
                                    new_ast,
                                    S::mk_exit(ExitStyle::Break, Some(current_loop.clone())),
                                );
                                break_targets.insert(current_loop.clone());
                            } else if !next_entries.contains(loop_label) {
                                new_ast = S::mk_append(
                                    new_ast,
                                    S::mk_exit(ExitStyle::Continue, Some(loop_label.clone())),
                                );
                                break_targets.insert(loop_label.clone());
                            }

                            new_ast.extend_span(*span);
                            Ok(new_ast)
                        }

                        GoTo(to) => panic!("Encountered GoTo({to:?}) in structured AST"),
                    }
                };

                S::mk_append(
                    body_ast,
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
                )
            }

            Loop { entries, body } => {
                let label = entries.first().expect("There must be at least one entry");
                let body = process_cfg(
                    body,
                    checked_entries,
                    entries,
                    &Some((label.clone(), &next_entries)),
                    break_targets,
                )?;
                S::mk_loop(Some(label.clone()), body)
            }

            Multiple { entries, branches } => {
                // If we have entries that aren't one of our branches, our `then` case needs to
                // be empty so that we fall through to the next structure. Otherwise we pull off
                // the first branch as the `then` case in order to satisfy the exhaustiveness
                // requirement for the generated `match`.
                let mut branch_entries = branches.keys();
                let then = if entries.iter().all(|entry| branches.contains_key(entry)) {
                    let then_entry = branch_entries
                        .next()
                        .expect("There must be at least one branch");
                    process_cfg(
                        &branches[then_entry],
                        checked_entries,
                        &next_entries,
                        loop_context,
                        break_targets,
                    )?
                } else {
                    S::empty()
                };

                let cases = branch_entries
                    .map(|entry| {
                        let stmts = process_cfg(
                            &branches[entry],
                            checked_entries,
                            &next_entries,
                            loop_context,
                            break_targets,
                        )?;
                        Ok((entry.clone(), stmts))
                    })
                    .collect::<TranslationResult<_>>()?;

                S::mk_goto_table(cases, then)
            }
        };

        i += 1;

        // Handle any followup multiple structures by wrapping the current structure's AST in a block.
        while let Some(Multiple { branches, .. }) = structures.get(i) {
            let next_entries = get_entries(i + 1);

            // Generate blocks as break targets for the remaining branches.
            for (branch_idx, entry) in sort_branches(branches).iter().enumerate() {
                let branch = &branches[entry];

                // Choose the next entries for the branch. For most of the branches we want to
                // say that there are no next entries, because we don't want one branch to flow
                // into another. But the last branch can fall through to the next entries that
                // follow this multiple.
                let empty = IndexSet::new();
                let next_entries = if branch_idx == branches.len() - 1 {
                    &next_entries
                } else {
                    &empty
                };

                let branch_ast = process_cfg::<S>(
                    branch,
                    checked_entries,
                    next_entries,
                    loop_context,
                    break_targets,
                )?;

                if break_targets.contains(entry) {
                    structure_ast = S::mk_block(entry.clone(), structure_ast);
                }

                structure_ast = S::mk_append(structure_ast, branch_ast);
            }

            i += 1;
        }

        // Check for a simple or loop after the multiple. If there is one, we need to
        // also wrap the current ast in a labeled block.
        match structures.get(i) {
            Some(Simple { entries, .. }) | Some(Loop { entries, .. }) => {
                for entry in entries {
                    if break_targets.contains(entry) {
                        structure_ast = S::mk_block(entry.clone(), structure_ast);
                    }
                }
            }

            Some(Multiple { .. }) => {
                unreachable!("We should have already handled followup multiples");
            }

            None => {}
        }

        ast = S::mk_append(ast, structure_ast);
    }

    Ok(ast)
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

            Block(lbl, body) => {
                // Make a labeled block.

                let (body, body_span) = self.to_stmt(*body, comment_store);

                let e =
                    mk().labelled_block_expr(mk().span(body_span).block(body), lbl.pretty_print());

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

#[cfg(test)]
mod tests {
    use super::*;

    type AST = StructuredAST<Box<Expr>, Pat, Label, Stmt>;

    fn label(id: u64) -> Label {
        Label::Synthetic(id)
    }

    fn check(mut input: AST, expected: AST) {
        cleanup_labels(&mut input, &None, &mut IndexSet::new());
        assert_eq!(input, expected);
    }

    #[test]
    fn test_removes_label_from_exit_targeting_current_loop() {
        // 'a: loop { break 'a; }  =>  loop { break; }
        check(
            AST::mk_loop(
                Some(label(1)),
                AST::mk_exit(ExitStyle::Break, Some(label(1))),
            ),
            AST::mk_loop(None, AST::mk_exit(ExitStyle::Break, None)),
        );
    }

    #[test]
    fn test_keeps_label_for_outer_loop_exit() {
        // 'a: loop { loop { break 'a; } }  =>  'a: loop { loop { break 'a; } }
        check(
            AST::mk_loop(
                Some(label(1)),
                AST::mk_loop(
                    Some(label(2)),
                    AST::mk_exit(ExitStyle::Break, Some(label(1))),
                ),
            ),
            AST::mk_loop(
                Some(label(1)),
                AST::mk_loop(None, AST::mk_exit(ExitStyle::Break, Some(label(1)))),
            ),
        );
    }

    #[test]
    fn test_removes_unused_loop_label() {
        // 'a: loop { break; }  =>  loop { break; }
        check(
            AST::mk_loop(Some(label(1)), AST::mk_exit(ExitStyle::Break, None)),
            AST::mk_loop(None, AST::mk_exit(ExitStyle::Break, None)),
        );
    }

    #[test]
    fn test_block_unlabeled_loop_with_labeled_break() {
        // 'a: { loop { break 'a; } }  =>  loop { break; }
        check(
            AST::mk_block(
                label(1),
                AST::mk_loop(None, AST::mk_exit(ExitStyle::Break, Some(label(1)))),
            ),
            AST::mk_loop(None, AST::mk_exit(ExitStyle::Break, None)),
        );
    }

    #[test]
    fn test_block_labeled_loop_merges_labels() {
        // 'a: { 'b: loop { break 'b; } }  =>  loop { break; }
        check(
            AST::mk_block(
                label(1),
                AST::mk_loop(
                    Some(label(2)),
                    AST::mk_exit(ExitStyle::Break, Some(label(2))),
                ),
            ),
            AST::mk_loop(None, AST::mk_exit(ExitStyle::Break, None)),
        );
    }

    #[test]
    fn test_block_labeled_loop_with_outer_exit() {
        // 'a: { 'b: loop { break 'a; } }  =>  loop { break; }
        check(
            AST::mk_block(
                label(1),
                AST::mk_loop(
                    Some(label(2)),
                    AST::mk_exit(ExitStyle::Break, Some(label(1))),
                ),
            ),
            AST::mk_loop(None, AST::mk_exit(ExitStyle::Break, None)),
        );
    }

    #[test]
    fn test_block_loop_with_external_exit() {
        // 'a: { 'b: loop { break 'c; } }  =>  loop { break 'c; }
        check(
            AST::mk_block(
                label(1),
                AST::mk_loop(
                    Some(label(2)),
                    AST::mk_exit(ExitStyle::Break, Some(label(3))),
                ),
            ),
            AST::mk_loop(None, AST::mk_exit(ExitStyle::Break, Some(label(3)))),
        );
    }

    #[test]
    fn test_block_nested_loops_with_labeled_break() {
        // Regression test: ensures block containing labeled loop preserves loop structure.
        // 'a: { 'b: loop { loop { break 'b; } } }  =>  'a: loop { loop { break 'a; } }
        check(
            AST::mk_block(
                label(1),
                AST::mk_loop(
                    Some(label(2)),
                    AST::mk_loop(
                        Some(label(3)),
                        AST::mk_exit(ExitStyle::Break, Some(label(2))),
                    ),
                ),
            ),
            AST::mk_loop(
                Some(label(1)),
                AST::mk_loop(None, AST::mk_exit(ExitStyle::Break, Some(label(1)))),
            ),
        );
    }

    #[test]
    fn test_nested_blocks_merge_labels() {
        // 'a: { 'b: { break 'b; } }  =>  'a: { break 'a; }
        check(
            AST::mk_block(
                label(1),
                AST::mk_block(label(2), AST::mk_exit(ExitStyle::Break, Some(label(2)))),
            ),
            AST::mk_block(label(1), AST::mk_exit(ExitStyle::Break, Some(label(1)))),
        );
    }

    #[test]
    fn test_nested_blocks_with_loop() {
        // 'a: { 'b: { loop { break 'b; } } }  =>  loop { break; }
        check(
            AST::mk_block(
                label(1),
                AST::mk_block(
                    label(2),
                    AST::mk_loop(
                        Some(label(3)),
                        AST::mk_exit(ExitStyle::Break, Some(label(2))),
                    ),
                ),
            ),
            AST::mk_loop(None, AST::mk_exit(ExitStyle::Break, None)),
        );
    }
}
