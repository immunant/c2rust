//! `ConstraintSet` and related definitions.
use log::debug;
use std::cmp;
use std::collections::btree_set::{self, BTreeSet};
use std::collections::Bound;
use std::collections::HashSet;
use std::collections::VecDeque;

use rustc_arena::DroplessArena;

use super::{ConcretePerm, PermVar, Var};

/// A permission expression.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Perm<'lty> {
    /// A concrete permission.
    Concrete(ConcretePerm),

    /// The minimum of some set of other permissions.  The slice should contain only atomic
    /// permissions, not `Min`s.
    // Weird ordering, but it's necessary for `perm_range` - we need a way to write down the
    // largest and smallest possible `Perm`s, and the largest/smallest `Min` is hard to get.
    Min(&'lty [Perm<'lty>]),

    // Wrappers around the various `PermVar`s.
    StaticVar(Var),
    SigVar(Var),
    InstVar(Var),
    LocalVar(Var),
}

impl<'lty, 'tcx> Perm<'lty> {
    pub fn read() -> Perm<'lty> {
        Perm::Concrete(ConcretePerm::Read)
    }

    pub fn write() -> Perm<'lty> {
        Perm::Concrete(ConcretePerm::Write)
    }

    pub fn move_() -> Perm<'lty> {
        Perm::Concrete(ConcretePerm::Move)
    }

    pub fn var(pv: PermVar) -> Perm<'lty> {
        match pv {
            PermVar::Static(v) => Perm::StaticVar(v),
            PermVar::Sig(v) => Perm::SigVar(v),
            PermVar::Inst(v) => Perm::InstVar(v),
            PermVar::Local(v) => Perm::LocalVar(v),
        }
    }

    pub fn as_var(&self) -> Option<PermVar> {
        match *self {
            Perm::Concrete(_) => None,
            Perm::Min(_) => None,
            Perm::StaticVar(v) => Some(PermVar::Static(v)),
            Perm::SigVar(v) => Some(PermVar::Sig(v)),
            Perm::InstVar(v) => Some(PermVar::Inst(v)),
            Perm::LocalVar(v) => Some(PermVar::Local(v)),
        }
    }

    /// Construct the minimum of two permissions.  This needs a reference to the arena, since it
    /// may need to allocate a new slice for `Min`.
    pub fn min(a: Perm<'lty>, b: Perm<'lty>, arena: &'lty DroplessArena) -> Perm<'lty> {
        debug!("finding min of {:?} and {:?}", a, b);
        match (a, b) {
            // A few easy cases
            (Perm::Concrete(ConcretePerm::Read), _) | (_, Perm::Concrete(ConcretePerm::Read)) => {
                Perm::read()
            }

            (Perm::Concrete(ConcretePerm::Move), p) => p,
            (p, Perm::Concrete(ConcretePerm::Move)) => p,

            (Perm::Min(ps1), Perm::Min(ps2)) => {
                let mut all = Vec::with_capacity(ps1.len() + ps2.len());
                all.extend(ps1.iter().cloned());
                for &p in ps2 {
                    if !all.contains(&p) {
                        all.push(p);
                    }
                }
                let all = if all.is_empty() {
                    &[] as &[_]
                } else {
                    arena.alloc_slice(&all)
                };
                debug!("nontrivial min: {:?}", all);
                Perm::Min(all)
            }

            (Perm::Min(ps), p) | (p, Perm::Min(ps)) => {
                if ps.contains(&p) {
                    Perm::Min(ps)
                } else {
                    let mut all = Vec::with_capacity(ps.len() + 1);
                    all.extend(ps.iter().cloned());
                    all.push(p);
                    let all = if all.is_empty() {
                        &[] as &[_]
                    } else {
                        arena.alloc_slice(&all)
                    };
                    debug!("nontrivial min: {:?}", all);
                    Perm::Min(all)
                }
            }

            (a, b) => {
                if a == b {
                    a
                } else {
                    let all = arena.alloc_slice(&[a, b]);
                    debug!("nontrivial min: {:?}", all);
                    Perm::Min(all)
                }
            }
        }
    }

    /// Check if `other` appears somewhere within `self`.  Note this checks syntactic presence
    /// only, not any kind of subtyping relation.
    pub fn contains(&self, other: Perm<'lty>) -> bool {
        if *self == other {
            return true;
        }
        match *self {
            Perm::Min(ps) => ps.iter().cloned().any(|p| p.contains(other)),
            _ => false,
        }
    }

    /// Modify `self` by replacing `old` with each element of `news` in turn, yielding each result
    /// to `callback`.
    pub fn for_each_replacement<F>(
        &self,
        arena: &'lty DroplessArena,
        old: Perm<'lty>,
        news: &[Perm<'lty>],
        mut callback: F,
    ) where
        F: FnMut(Perm<'lty>),
    {
        if *self == old {
            // Easy case
            for &new in news {
                callback(new);
            }
            return;
        }

        let self_ps = match *self {
            Perm::Min(ps) => ps,
            _ => {
                // Easy case - `self` is atomic and not equal to `old`.  There's no replacement to
                // be done.
                callback(*self);
                return;
            }
        };

        let mut buf = self_ps.to_owned();
        buf.retain(|&p| p != old);
        let base_len = buf.len();

        for &new in news {
            match new {
                Perm::Min(ps) => {
                    for &p in ps {
                        if !buf.contains(&p) {
                            buf.push(p);
                        }
                    }
                }
                _ => {
                    if !buf.contains(&new) {
                        buf.push(new);
                    }
                }
            }

            if buf.len() == 1 {
                callback(buf[0]);
            } else {
                callback(Perm::Min(arena.alloc_slice(&buf)));
            }

            buf.truncate(base_len);
        }
    }

    /// Iterator over each atomic (non-`Min`) permission that appears in `self`.
    pub fn for_each_atom<F: FnMut(Perm<'lty>)>(&self, callback: &mut F) {
        match *self {
            Perm::Min(ps) => {
                for &p in ps {
                    p.for_each_atom(callback);
                }
            }
            _ => callback(*self),
        }
    }
}

/// A set of constraints over permission expressions, of the form `p1 <= p2`.
///
/// Note that most of the more complex operations are imprecise (unsound) in certain cases
/// involving `Min`.  Fortunately, these cases seem not to come up often in practice.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ConstraintSet<'lty> {
    less: BTreeSet<(Perm<'lty>, Perm<'lty>)>,
    greater: BTreeSet<(Perm<'lty>, Perm<'lty>)>,
}

/// Return a pair of bounds, suitable for use with `BTreeSet::range`, covering all pairs of
/// permissions whose first element is `p`.
fn perm_range<'lty, 'tcx>(
    p: Perm<'lty>,
) -> (
    Bound<(Perm<'lty>, Perm<'lty>)>,
    Bound<(Perm<'lty>, Perm<'lty>)>,
) {
    (
        Bound::Included((p, Perm::read())),
        Bound::Included((p, Perm::LocalVar(Var(!0)))),
    )
}

impl<'lty, 'tcx> ConstraintSet<'lty> {
    pub fn new() -> ConstraintSet<'lty> {
        ConstraintSet {
            less: BTreeSet::new(),
            greater: BTreeSet::new(),
        }
    }

    pub fn iter(&self) -> btree_set::Iter<(Perm<'lty>, Perm<'lty>)> {
        self.less.iter()
    }

    pub fn add(&mut self, a: Perm<'lty>, b: Perm<'lty>) {
        self.less.insert((a, b));
        self.greater.insert((b, a));
    }

    /// Add all constraints from `other` to `self`.
    pub fn import(&mut self, other: &ConstraintSet<'lty>) {
        debug!("IMPORT {} constraints", other.less.len());
        self.less
            .extend(other.less.iter().cloned().filter(|&(ref a, ref b)| {
                debug!("IMPORT CONSTRAINT: {:?} <= {:?}", a, b);
                true
            }));
        self.greater.extend(other.greater.iter().cloned());
    }

    /// For each constraint in `other`, substitute all atomic permissions using the callback `f`,
    /// then add the constraint to `self`.
    pub fn import_substituted<F>(
        &mut self,
        other: &ConstraintSet<'lty>,
        arena: &'lty DroplessArena,
        f: F,
    ) where
        F: Fn(Perm<'lty>) -> Perm<'lty>,
    {
        debug!("IMPORT {} constraints (substituted)", other.less.len());

        let subst_one = |p| match p {
            Perm::Min(ps) => {
                let mut buf = Vec::with_capacity(ps.len());
                for &p in ps {
                    let q = f(p);
                    if !buf.contains(&q) {
                        buf.push(q);
                    }
                }
                Perm::Min(arena.alloc_slice(&buf))
            }
            p => f(p),
        };

        for &(a, b) in other.less.iter() {
            let (a2, b2) = (subst_one(a), subst_one(b));
            debug!(
                "IMPORT CONSTRAINT: {:?} <= {:?} (substituted from {:?} <= {:?})",
                a2, b2, a, b
            );
            self.add(a2, b2);
        }
    }

    /// Clone `self`, substituting each atomic permission using the callback `f`.
    pub fn clone_substituted<F>(&self, arena: &'lty DroplessArena, f: F) -> ConstraintSet<'lty>
    where
        F: Fn(Perm<'lty>) -> Perm<'lty>,
    {
        let mut new_cset = ConstraintSet::new();
        new_cset.import_substituted(self, arena, f);
        new_cset
    }

    /// Run a breadth-first search over `map`, starting from `p`.  Runs callback `f` on each
    /// encountered permission.
    fn traverse_constraints<F>(map: &BTreeSet<(Perm<'lty>, Perm<'lty>)>, p: Perm<'lty>, mut f: F)
    where
        F: FnMut(Perm<'lty>) -> bool,
    {
        let mut seen = HashSet::new();
        let mut queue = VecDeque::new();

        if f(p) {
            seen.insert(p);
            queue.push_back(p);
        }

        while let Some(cur) = queue.pop_front() {
            for &(_, next) in map.range(perm_range(cur)) {
                if !seen.contains(&next) {
                    if f(next) {
                        seen.insert(next);
                        queue.push_back(next);
                    }
                }
            }
        }
    }

    /// Iterate over all permissions less than `p`.
    ///
    /// This only traverses chains of `q <= p`, `r <= q`, etc.  It doesn't do anything intelligent
    /// regarding `Min`.
    pub fn for_each_less_than<F>(&self, p: Perm<'lty>, f: F)
    where
        F: FnMut(Perm<'lty>) -> bool,
    {
        Self::traverse_constraints(&self.greater, p, f);
    }

    /// Iterate over all permissions greater than `p`.
    ///
    /// This only traverses chains of `p <= q`, `q <= r`, etc.  It doesn't do anything intelligent
    /// regarding `Min`.
    pub fn for_each_greater_than<F>(&self, p: Perm<'lty>, f: F)
    where
        F: FnMut(Perm<'lty>) -> bool,
    {
        Self::traverse_constraints(&self.less, p, f);
    }

    /// Obtain a concrete lower bound on `p`.  Replacing `p` with any concrete permission less than
    /// `lower_bound(p)` is guaranteed to make the constraint set unsatisfiable.
    ///
    /// This function may return a lower result than necessary due to imprecise reasoning about
    /// `Min`.
    pub fn lower_bound(&self, p: Perm<'lty>) -> ConcretePerm {
        match p {
            Perm::Concrete(p) => return p,
            _ => {}
        }

        let mut bound = ConcretePerm::Read;
        self.for_each_less_than(p, |p| match p {
            Perm::Concrete(p) => {
                bound = cmp::max(bound, p);
                false
            }
            _ => true,
        });

        bound
    }

    /// Obtain a concrete upper bound on `p`.  Replacing `p` with any concrete permission greater
    /// than `upper_bound(p)` is guaranteed to make the constraint set unsatisfiable.
    ///
    /// This function may return a higher result than necessary due to imprecise reasoning about
    /// `Min`.
    pub fn upper_bound(&self, p: Perm<'lty>) -> ConcretePerm {
        match p {
            Perm::Concrete(p) => return p,
            _ => {}
        }

        let mut bound = ConcretePerm::Move;
        self.for_each_greater_than(p, |p| match p {
            Perm::Concrete(p) => {
                bound = cmp::min(bound, p);
                false
            }
            _ => true,
        });

        bound
    }

    /// Given an assignment of concrete permission values to a subset of the variables, check
    /// whether any constraints are violated under the partial assignment.  Returns `false` if a
    /// constraint is violated, or `true` if all constraints appear to be satisfiable.
    ///
    /// Note this function is only guaranteed to be accurate for complete assignments.  On
    /// (strictly) partial assignments, it may report that a satisfying assignment is possible when
    /// it's not, but never the other way around.
    pub fn check_partial_assignment<F>(&self, eval: F) -> bool
    where
        F: Fn(Perm<'lty>) -> Option<ConcretePerm>,
    {
        /// Evaluate a permission, recursing into `Perm::Min`s.  Returns the computed permission
        /// value along with two boolean flags `any_missing` and `all_missing`.
        fn eval_rec<'lty, 'tcx, F>(p: Perm<'lty>, eval: &F) -> (ConcretePerm, bool, bool)
        where
            F: Fn(Perm<'lty>) -> Option<ConcretePerm>,
        {
            match p {
                Perm::Concrete(c) => (c, false, false),
                Perm::Min(ps) => {
                    let mut min = ConcretePerm::Move;
                    let mut any_missing = false;
                    let mut all_missing = true;

                    for &p in ps {
                        let (c, any, all) = eval_rec(p, eval);
                        min = cmp::min(min, c);
                        any_missing |= any;
                        all_missing &= all;
                    }
                    (min, any_missing, all_missing)
                }
                p => {
                    if let Some(c) = eval(p) {
                        (c, false, false)
                    } else {
                        (ConcretePerm::Move, true, true)
                    }
                }
            }
        }

        for &(a, b) in &self.less {
            let (a, a_any, a_all) = eval_rec(a, &eval);
            let (b, _b_any, b_all) = eval_rec(b, &eval);

            if a <= b {
                continue;
            }

            if a_all || b_all {
                // We have no info at all about one of the sides, so skip this constraint.
                continue;
            }

            // If `a_any` is set, then further assignments might make the constraint "more
            // satisfiable", by lowering the result of a `min` on the LHS.  Conversely, if `b_any`
            // is set, further assignments might make it less satisfiable.
            if a_any {
                // Constraint is violated now, but could be repaired by further assignments.
                continue;
            }

            debug!("Constraint {:?} <= {:?} is not satisfiable", a, b);

            return false;
        }

        true
    }

    /// Obtain an editing cursor for this constraint set.
    pub fn edit<'a>(&'a mut self) -> EditConstraintSet<'a, 'lty> {
        let to_visit = self.less.iter().cloned().collect();
        EditConstraintSet {
            cset: self,
            to_visit,
        }
    }

    /// Iterate over each atomic permission in each constraint.
    pub fn for_each_perm<F: FnMut(Perm<'lty>)>(&self, mut f: F) {
        for &(a, b) in &self.less {
            a.for_each_atom(&mut f);
            b.for_each_atom(&mut f);
        }
    }
}

/// Editing cursor, for visiting every constraint while adding/removing as you go.
pub struct EditConstraintSet<'a, 'lty> {
    /// The underlying constraint set.
    cset: &'a mut ConstraintSet<'lty>,

    /// Queue of constraints that have yet to be visited.
    to_visit: VecDeque<(Perm<'lty>, Perm<'lty>)>,
}

impl<'a, 'lty> EditConstraintSet<'a, 'lty> {
    /// Obtain the next constraint if there are any left to be processed.
    pub fn next(&mut self) -> Option<(Perm<'lty>, Perm<'lty>)> {
        while let Some((a, b)) = self.to_visit.pop_front() {
            if self.cset.less.contains(&(a, b)) {
                return Some((a, b));
            }
        }
        None
    }

    /// Add a new constraint.  If the constraint didn't already exist, it will be queued up to be
    /// visited in the future.
    pub fn add(&mut self, a: Perm<'lty>, b: Perm<'lty>) {
        if self.cset.less.contains(&(a, b)) {
            return;
        }
        self.cset.less.insert((a, b));
        self.cset.greater.insert((b, a));
        self.to_visit.push_back((a, b));
    }

    /// Add a constraint, but never queue it up for future visiting.
    pub fn add_no_visit(&mut self, a: Perm<'lty>, b: Perm<'lty>) {
        if self.cset.less.contains(&(a, b)) {
            return;
        }
        self.cset.less.insert((a, b));
        self.cset.greater.insert((b, a));
    }

    /// Remove a constraint.
    pub fn remove(&mut self, a: Perm<'lty>, b: Perm<'lty>) {
        self.cset.less.remove(&(a, b));
        self.cset.greater.remove(&(b, a));
        // If it remains in `to_visit`, it will be skipped by `next`.
    }
}

impl<'lty, 'tcx> ConstraintSet<'lty> {
    /// Remove constraints that are obviously useless, like `READ <= p`.
    pub fn remove_useless(&mut self) {
        let mut edit = self.edit();

        while let Some((a, b)) = edit.next() {
            let remove = match (a, b) {
                (Perm::Concrete(_), Perm::Concrete(_)) => true,
                (Perm::Concrete(ConcretePerm::Read), _) => true,
                (_, Perm::Concrete(ConcretePerm::Move)) => true,
                _ => a == b,
            };
            if remove {
                debug!("remove: {:?} <= {:?}", a, b);
                edit.remove(a, b);
            }
        }
    }

    /// Simplify `a <= min(b1, b2)` into `a <= b1, a <= b2`.
    pub fn expand_min_rhs(&mut self) {
        let mut edit = self.edit();

        while let Some((a, b)) = edit.next() {
            match b {
                Perm::Min(ps) => {
                    debug!("expand: {:?} <= {:?}", a, b);
                    edit.remove(a, b);
                    for &p in ps {
                        edit.add(a, p);
                    }
                }
                _ => {}
            }
        }
    }

    /// Simplify `min(...) <= ...` constraints as much as possible.  Unlike `... <= min(...)`, it
    /// may not always be possible to completely eliminate such constraints.
    pub fn simplify_min_lhs(&mut self, arena: &'lty DroplessArena) {
        let mut edit = self.edit();

        'next: while let Some((a, b)) = edit.next() {
            let ps = match a {
                Perm::Min(ps) => ps,
                _ => continue,
            };

            if ps.is_empty() {
                // Should never happen, but just in case...
                edit.remove(a, b);
                continue;
            }

            // We now have `min(p_0, p_1, ...) <= b`.  We want to reduce the set of `p_i`s as
            // much as possible, ideally down to a single element.  The approach taken here
            // (which is quite inefficient) is to collect, for each `p_i`, the set of `q`s
            // where `p_i <= q`.  Then we query those sets to figure out what `p_i`s can be
            // removed.

            let mut greater_sets = Vec::with_capacity(ps.len());
            for &p in ps {
                let mut seen = HashSet::new();
                let mut queue = VecDeque::new();
                queue.push_back(p);
                while let Some(cur) = queue.pop_front() {
                    for &(_, next) in edit.cset.less.range(perm_range(cur)) {
                        if !seen.contains(&next) {
                            seen.insert(next);
                            queue.push_back(next);
                        }
                    }
                }
                greater_sets.push(seen);
            }

            // Now we can make some queries into `greater_sets`.  The two things we want to
            // check are:
            //  (1) If `p_i <= p_j`, then `p_j` can be removed.
            //  (2) If `p_i <= b`, then the entire constraint can be discarded.

            let mut to_remove = HashSet::new();
            for (i, &pi) in ps.iter().enumerate() {
                // This check handles cycles.  Suppose `a <= b <= c <= a` and `d <= e`.  We'd
                // like to replace `min(a, b, c, d, e)` with `min(a, d)`.  Without this check,
                // we would end up with `min()`, because `a` eliminates `b` and `c`, `b` and
                // `c` eliminate `a`, and `d` and `e` remove each other.  This check doesn't
                // cause us to miss any valid removals because if `a <= b` and `b <= x` then
                // also `a <= x`.
                if to_remove.contains(&i) {
                    continue;
                }

                for (j, &pj) in ps.iter().enumerate() {
                    if i != j && greater_sets[i].contains(&pj) {
                        to_remove.insert(j);
                    }
                }

                if greater_sets[i].contains(&b) {
                    debug!("remove {:?} <= {:?} ({:?} <= {:?})", a, b, pi, b);
                    edit.remove(a, b);
                    continue 'next;
                }
            }

            assert!(
                to_remove.len() < ps.len(),
                "tried to remove all arguments of `min`"
            );
            if to_remove.len() == ps.len() - 1 {
                // `min(p)` is the same as just `p`.
                edit.remove(a, b);
                let (_, p) = ps
                    .iter()
                    .cloned()
                    .enumerate()
                    .filter(|&(i, _)| !to_remove.contains(&i))
                    .next()
                    .unwrap();
                debug!("replace {:?} <= {:?} with {:?} <= {:?}", a, b, p, b);
                edit.add(p, b);
            } else if to_remove.len() > 0 {
                edit.remove(a, b);
                let ps = ps
                    .iter()
                    .cloned()
                    .enumerate()
                    .filter(|&(i, _)| !to_remove.contains(&i))
                    .map(|(_, p)| p)
                    .collect::<Vec<_>>();
                let new_min = Perm::Min(arena.alloc_slice(&ps));
                debug!("replace {:?} <= {:?} with {:?} <= {:?}", a, b, new_min, b);
                edit.add(new_min, b);
            }
            // Otherwise, to_remove == 0, meaning we don't have any changes to apply.
        }
    }

    /// Simplify the constraint set as best we can.
    pub fn simplify(&mut self, arena: &'lty DroplessArena) {
        self.remove_useless();
        self.expand_min_rhs();
        self.simplify_min_lhs(arena);
    }

    /// Eliminate constraints involving permissions on which `f(p)` returns false, while
    /// maintaining relationships between other permissions.  For example, when removing `q` from
    /// `{p <= q, q <= r}`, we would add `p <= r` before removing the other two constraints.
    ///
    /// This may be imprecise if a removed permission appears as an argument of a `Min`.  Simplify
    /// the constraint set first to remove as many `Min`s as possible before using this function.
    pub fn retain_perms<F>(&mut self, arena: &'lty DroplessArena, filter: F)
    where
        F: Fn(Perm<'lty>) -> bool,
    {
        // Collect all atomic permissions that appear in the constraint set.
        let mut atomic_perms = HashSet::new();
        fn collect_atomic<'lty, 'tcx>(p: Perm<'lty>, dest: &mut HashSet<Perm<'lty>>) {
            match p {
                Perm::Min(ps) => {
                    for &p in ps {
                        collect_atomic(p, dest);
                    }
                }
                _ => {
                    dest.insert(p);
                }
            }
        }
        for &(p1, p2) in &self.less {
            collect_atomic(p1, &mut atomic_perms);
            collect_atomic(p2, &mut atomic_perms);
        }

        // Add edges routing around each removed permission.
        for &p in &atomic_perms {
            if filter(p) {
                continue;
            }

            debug!("removing perm {:?}", p);

            // Perms less than `p`, and perms greater than `p`.
            let less = self
                .greater
                .range(perm_range(p))
                .map(|&(_, b)| b)
                .filter(|&b| b != p)
                .collect::<Vec<_>>();
            let greater = self
                .less
                .range(perm_range(p))
                .map(|&(_, b)| b)
                .filter(|&b| b != p)
                .collect::<Vec<_>>();
            debug!("    less: {:?}", less);
            debug!("    greater: {:?}", greater);

            let mut edit = self.edit();
            while let Some((a, b)) = edit.next() {
                if !a.contains(p) && !b.contains(p) {
                    continue;
                }

                // TODO: This may be more aggressive than necessary on `min(removed)` permissions.
                // In those cases, we should delete just the removed permission(s) from the `Min`,
                // and only delete the whole constraint if the `Min` winds up empty.
                debug!("  remove {:?} <= {:?}", a, b);
                edit.remove(a, b);
                a.for_each_replacement(arena, p, &less, |a| {
                    b.for_each_replacement(arena, p, &greater, |b| {
                        debug!("    replacement: {:?} <= {:?}", a, b);
                        edit.add_no_visit(a, b);
                    });
                });
            }
        }
    }
}
