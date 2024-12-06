//! Analysis to find the last use of a MIR local.  For non-`Copy` types, the last use can be made
//! into a move instead of a clone or borrow.
//!
//! In terms of liveness analysis, a "last use" is a statement where a variable transitions from
//! live to dead as a result of its use in the statement.  A variable can also become dead due to
//! control flow: in `if f(p) { g(p); }`, the variable `p` is still live after evaluating `f(p)`,
//! but becomes dead upon taking the `else` branch, which contains no more uses of `p`.  In this
//! example, `g(p)` is a last use, but `f(p)` is not (and in the `else` case, no last use of `p`
//! will be encountered during execution).
//!
//! We specifically use this when handling borrows of non-`Copy` pointers like `Option<&mut T>`.
//! At most use sites, we produce `ptr.as_deref_mut().unwrap() ...`, which borrows `ptr` instead of
//! moving it.  However, this construction produces a reference with a shorter lifetime, matching
//! the lifetime of the local `ptr`.  When returning a borrow from a function, we instead want to
//! emit `ptr.unwrap() ...`, which consumes `ptr` but produces a reference with the same lifetime
//! as the reference in `ptr`.

use std::collections::{HashMap, VecDeque};

use rustc_index::bit_set::BitSet;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::traversal;
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, Body, Local, Location, Operand, Place, Rvalue, Statement,
    StatementKind, Terminator, TerminatorKind,
};

/// A single MIR `Statement` or `Terminator` may refer to multiple `Place`s in different
/// `Operand`s.  This type identifies a particular `Place` of interest within the `Statement`
/// or`Terminator`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum WhichPlace {
    Lvalue,
    Operand(usize),
}

#[derive(Clone, Debug, Default)]
pub struct LastUse {
    /// `(location, which_place)` is present in this map if the indicated `Place` is the last use
    /// of the value currently in that `Place`'s `Local`.
    ///
    /// Note that there may be more than one "last use" for a given local.  If a variable `p` is
    /// used, overwritten, and used again, then both uses may be marked as last uses: the first
    /// value stored in `p` is used for the last time, then a new value is stored in `p`, and the
    /// second value is also used (possibly for the last time).  When there is branching control
    /// flow, such as `if cond { f(p); } else { g(p); }`, then there may be a last use in each
    /// branch.
    ///
    /// Also, there may be zero last uses for a given local.  In `while f(p) { g(p); }`, the use
    /// `g(p)` can't be a last use because `f(p)` will be called afterward, and `f(p)` can't be a
    /// last use because `g(p)` might be called afterward.
    ///
    /// The `Local` value is used only for debugging; it isn't needed for generating rewrites.
    set: HashMap<(Location, WhichPlace), Local>,
}

impl LastUse {
    pub fn is_last_use(&self, loc: Location, which: WhichPlace) -> bool {
        self.set.contains_key(&(loc, which))
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (Location, WhichPlace, Local)> + 'a {
        self.set
            .iter()
            .map(|(&(loc, which), &local)| (loc, which, local))
    }
}

struct Action {
    kind: ActionKind,
    local: Local,
    location: Location,
    which_place: WhichPlace,
}

enum ActionKind {
    /// The local was written.  This only counts direct writes that update the entire local, like
    /// `_1 = ...`.  Writes to fields or indirect writes through a reference don't count as `Def`s.
    Def,
    Use,
}

struct ActionsBuilder {
    actions: Vec<Action>,
}

impl ActionsBuilder {
    pub fn new() -> ActionsBuilder {
        ActionsBuilder {
            actions: Vec::new(),
        }
    }

    pub fn push_action(&mut self, act: Action) {
        self.actions.push(act);
    }

    pub fn finish(self) -> Vec<Action> {
        self.actions
    }

    pub fn push_place_use(&mut self, pl: Place, loc: Location, which: WhichPlace) {
        self.push_action(Action {
            kind: ActionKind::Use,
            local: pl.local,
            location: loc,
            which_place: which,
        });
    }

    pub fn push_lvalue_place(&mut self, pl: Place, loc: Location) {
        if pl.projection.is_empty() {
            self.push_action(Action {
                kind: ActionKind::Def,
                local: pl.local,
                location: loc,
                which_place: WhichPlace::Lvalue,
            });
        } else {
            self.push_place_use(pl, loc, WhichPlace::Lvalue);
        }
    }

    pub fn push_operand(&mut self, op: &Operand, loc: Location, which: WhichPlace) {
        match *op {
            Operand::Copy(pl) | Operand::Move(pl) => self.push_place_use(pl, loc, which),
            Operand::Constant(_) => {}
        }
    }

    pub fn push_rvalue(&mut self, rv: &Rvalue, loc: Location) {
        match *rv {
            Rvalue::Use(ref op) => {
                self.push_operand(op, loc, WhichPlace::Operand(0));
            }
            Rvalue::Repeat(ref op, _) => {
                self.push_operand(op, loc, WhichPlace::Operand(0));
            }
            Rvalue::Ref(_rg, _kind, pl) => {
                self.push_place_use(pl, loc, WhichPlace::Operand(0));
            }
            Rvalue::ThreadLocalRef(_) => {}
            Rvalue::AddressOf(_mutbl, pl) => {
                self.push_place_use(pl, loc, WhichPlace::Operand(0));
            }
            Rvalue::Len(pl) => {
                self.push_place_use(pl, loc, WhichPlace::Operand(0));
            }
            Rvalue::Cast(_kind, ref op, _ty) => {
                self.push_operand(op, loc, WhichPlace::Operand(0));
            }
            Rvalue::BinaryOp(_bin_op, ref x) | Rvalue::CheckedBinaryOp(_bin_op, ref x) => {
                let (ref a, ref b) = **x;
                self.push_operand(a, loc, WhichPlace::Operand(0));
                self.push_operand(b, loc, WhichPlace::Operand(1));
            }
            Rvalue::NullaryOp(_null_op, _ty) => {}
            Rvalue::UnaryOp(_un_op, ref op) => {
                self.push_operand(op, loc, WhichPlace::Operand(0));
            }
            Rvalue::Discriminant(pl) => {
                self.push_place_use(pl, loc, WhichPlace::Operand(0));
            }
            Rvalue::Aggregate(_, ref ops) => {
                for (i, op) in ops.iter().enumerate() {
                    self.push_operand(op, loc, WhichPlace::Operand(i));
                }
            }
            Rvalue::ShallowInitBox(ref op, _ty) => {
                self.push_operand(op, loc, WhichPlace::Operand(0));
            }
            Rvalue::CopyForDeref(pl) => {
                self.push_place_use(pl, loc, WhichPlace::Operand(0));
            }
        }
    }

    pub fn push_statement(&mut self, stmt: &Statement, loc: Location) {
        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;
                // The RHS is evaluated before the LHS.
                self.push_rvalue(rv, loc);
                self.push_lvalue_place(pl, loc);
            }
            StatementKind::FakeRead(..) => {}
            StatementKind::SetDiscriminant {
                ref place,
                variant_index: _,
            } => {
                self.push_place_use(**place, loc, WhichPlace::Operand(0));
            }
            StatementKind::Deinit(ref place) => {
                self.push_place_use(**place, loc, WhichPlace::Operand(0));
            }
            StatementKind::StorageLive(..) => {}
            StatementKind::StorageDead(..) => {}
            // `Retag` shouldn't appear: "These statements are currently only interpreted by miri
            // and only generated when `-Z mir-emit-retag` is passed."
            StatementKind::Retag(..) => panic!("unexpected StatementKind::Retag"),
            StatementKind::AscribeUserType(..) => {}
            StatementKind::Coverage(..) => {}
            StatementKind::CopyNonOverlapping(ref cno) => {
                self.push_operand(&cno.src, loc, WhichPlace::Operand(0));
                self.push_operand(&cno.dst, loc, WhichPlace::Operand(0));
                self.push_operand(&cno.count, loc, WhichPlace::Operand(0));
            }
            StatementKind::Nop => {}
        }
    }

    pub fn push_terminator(&mut self, term: &Terminator, loc: Location) {
        match term.kind {
            TerminatorKind::Goto { .. } => {}
            TerminatorKind::SwitchInt { ref discr, .. } => {
                self.push_operand(discr, loc, WhichPlace::Operand(0));
            }
            TerminatorKind::Resume => {}
            TerminatorKind::Abort => {}
            TerminatorKind::Return => {}
            TerminatorKind::Unreachable => {}
            // We ignore automatically-inserted `Drop`s, since the `Drop` may be eliminated if a
            // previous use is converted to a move.
            TerminatorKind::Drop { .. } => {}
            // `DropAndReplace` is used for some assignments.
            TerminatorKind::DropAndReplace {
                place, ref value, ..
            } => {
                self.push_operand(value, loc, WhichPlace::Operand(0));
                self.push_lvalue_place(place, loc);
            }
            TerminatorKind::Call {
                ref func, ref args, ..
            } => {
                self.push_operand(func, loc, WhichPlace::Operand(0));
                for (i, op) in args.iter().enumerate() {
                    self.push_operand(op, loc, WhichPlace::Operand(i + 1));
                }
            }
            // TODO: Handle `Assert`.  For now We ignore it because it isn't generated for any
            // pointer-related operations, and we don't currently care about `LastUse` results for
            // non-pointers.
            TerminatorKind::Assert { .. } => {}
            TerminatorKind::Yield { .. } => panic!("unsupported TerminatorKind::Yield"),
            TerminatorKind::GeneratorDrop => {}
            TerminatorKind::FalseEdge { .. } => {}
            TerminatorKind::FalseUnwind { .. } => {}
            TerminatorKind::InlineAsm { .. } => panic!("unsupported TerminatorKind::InlineAsm"),
        }
    }
}

fn calc_block_actions(bb: BasicBlock, bb_data: &BasicBlockData) -> Vec<Action> {
    let mut builder = ActionsBuilder::new();

    for (i, stmt) in bb_data.statements.iter().enumerate() {
        builder.push_statement(
            stmt,
            Location {
                block: bb,
                statement_index: i,
            },
        );
    }

    if let Some(ref term) = bb_data.terminator {
        builder.push_terminator(
            term,
            Location {
                block: bb,
                statement_index: bb_data.statements.len(),
            },
        );
    }

    builder.finish()
}

pub fn calc_last_use(mir: &Body) -> LastUse {
    // Build a list of relevant actions for each block.  An action is an access (`Use`) or a write
    // (`Def`) of a local variable.
    let block_actions: IndexVec<BasicBlock, Vec<Action>> = mir
        .basic_blocks
        .iter_enumerated()
        .map(|(bb, data)| calc_block_actions(bb, data))
        .collect();

    // Compute liveness on entry for each block.
    let mut entry: IndexVec<BasicBlock, BitSet<Local>> = IndexVec::from_elem_n(
        BitSet::new_empty(mir.local_decls.len()),
        mir.basic_blocks.len(),
    );
    let mut dirty: BitSet<BasicBlock> = BitSet::new_empty(mir.basic_blocks.len());
    let mut queue = VecDeque::new();
    // In some cases, `postorder` doesn't return all the blocks.  We add any leftover blocks
    // afterward.
    for (bb, _) in traversal::postorder(mir).chain(mir.basic_blocks.iter_enumerated()) {
        if dirty.insert(bb) {
            queue.push_back(bb);
        }
    }
    eprintln!("queue = {queue:?}");

    while let Some(bb) = queue.pop_front() {
        let mut live = BitSet::new_empty(mir.local_decls.len());
        // Start at the end of the block and work backwards.  A local is live on exit from the
        // block if it's live in at least one successor.
        for succ in mir.basic_blocks[bb].terminator().successors() {
            live.union(&entry[succ]);
        }
        // Apply the effects of the block.
        for act in block_actions[bb].iter().rev() {
            // A variable is live if it may be read before it is written.
            match act.kind {
                ActionKind::Use => {
                    // This is a read.  The variable is definitely live prior to this.
                    live.insert(act.local);
                }
                ActionKind::Def => {
                    // This is a write.  The variable is definitely not live prior to this.
                    live.remove(act.local);
                }
            }
        }
        // If the live variables on entry changed for this block, recompute its predecessors.
        let changed = live != entry[bb];
        eprintln!("update {bb:?}: {:?} -> {:?}", entry[bb], live);
        entry[bb] = live;
        dirty.remove(bb);
        if changed {
            for &pred in &mir.basic_blocks.predecessors()[bb] {
                if dirty.insert(pred) {
                    eprintln!("  enqueue {pred:?}");
                    queue.push_back(pred);
                }
            }
        }
    }

    debug_assert!(dirty.is_empty());

    // Make a final pass over each block to find the `LastUse` of each variable.  Any time we see
    // an `ActionKind::Use` where the local is live before but dead after, we record that action as
    // a "last use".
    let mut last_use = LastUse {
        set: HashMap::new(),
    };
    for (bb, actions) in block_actions.iter_enumerated() {
        let mut live = BitSet::new_empty(mir.local_decls.len());
        for succ in mir.basic_blocks[bb].terminator().successors() {
            live.union(&entry[succ]);
        }

        for act in actions.iter().rev() {
            match act.kind {
                ActionKind::Use => {
                    if live.insert(act.local) {
                        // We're processing the actions in reverse order, and this local was dead
                        // (not present in the `live` set) before this `insert` call, and now is
                        // live.  This means during forward execution of the block, the local is
                        // live before this action and dead afterward.
                        last_use
                            .set
                            .insert((act.location, act.which_place), act.local);
                    }
                }
                ActionKind::Def => {
                    live.remove(act.local);
                }
            }
        }

        debug_assert_eq!(
            live, entry[bb],
            "calc_last_use failed to reach fixpoint for {bb:?}"
        );
    }

    last_use
}
