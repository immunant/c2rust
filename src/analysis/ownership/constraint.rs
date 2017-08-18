//! `ConstraintSet` and related definitions.
use std::cmp;
use std::collections::Bound;
use std::collections::btree_set::{self, BTreeSet};
use std::collections::HashSet;
use std::collections::VecDeque;

use arena::DroplessArena;

use super::{ConcretePerm, Var};


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Perm<'tcx> {
    Concrete(ConcretePerm),

    /// The minimum of some set of other permissions.  The slice should contain only atomic
    /// permissions, not `Min`s.
    // Weird ordering, but it's necessary for `perm_range` - we need a way to write down the
    // largest and smallest possible `Perm`s, and the largest/smallest `Min` is hard to get.
    Min(&'tcx [Perm<'tcx>]),

    /// "Static" variables appear in the types of non-function items.  This includes `static` items
    /// as well as `struct`s and other ADTs.  Constraints on static vars are inferred from their
    /// usage inside functions.
    StaticVar(Var),

    /// "Signature" variables appear in the signatures of function items.  Constraints on sig vars
    /// are inferred from the body of the function in question.
    SigVar(Var),

    /// "Instantiation" variables appear in the instantiations of function signatures inside other
    /// functions.  They are left intact during the initial summary generation, to be filled in
    /// during a later phase of the analysis.
    InstVar(Var),

    /// "Local" variables appear in the types of temporaries.  Constraints on local vars are
    /// produced while analyzing a function, and are simplified away when the function's constraint
    /// generation is done.
    LocalVar(Var),
}

impl<'tcx> Perm<'tcx> {
    pub fn read() -> Perm<'tcx> {
        Perm::Concrete(ConcretePerm::Read)
    }

    pub fn write() -> Perm<'tcx> {
        Perm::Concrete(ConcretePerm::Write)
    }

    pub fn move_() -> Perm<'tcx> {
        Perm::Concrete(ConcretePerm::Move)
    }

    /// Check if `other` appears somewhere within `self`.  Note this checks syntactic presence
    /// only, not any kind of subtyping relation.
    pub fn contains(&self, other: Perm<'tcx>) -> bool {
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
    pub fn for_each_replacement<F>(&self,
                                   arena: &'tcx DroplessArena,
                                   old: Perm<'tcx>,
                                   news: &[Perm<'tcx>],
                                   mut callback: F)
            where F: FnMut(Perm<'tcx>) {
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
            },
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
                },
                _ => {
                    if !buf.contains(&new) {
                        buf.push(new);
                    }
                },
            }

            if buf.len() == 1 {
                callback(buf[0]);
            } else {
                callback(Perm::Min(arena.alloc_slice(&buf)));
            }

            buf.truncate(base_len);
        }
    }

    pub fn for_each_atom<F: FnMut(Perm<'tcx>)>(&self, callback: &mut F) {
        match *self {
            Perm::Min(ps) => {
                for &p in ps {
                    p.for_each_atom(callback);
                }
            },
            _ => callback(*self),
        }
    }
}


#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ConstraintSet<'tcx> {
    less: BTreeSet<(Perm<'tcx>, Perm<'tcx>)>,
    greater: BTreeSet<(Perm<'tcx>, Perm<'tcx>)>,
}

fn perm_range(p: Perm) -> (Bound<(Perm, Perm)>, Bound<(Perm, Perm)>) {
    (Bound::Included((p, Perm::read())),
     Bound::Included((p, Perm::LocalVar(Var(!0)))))
}

impl<'tcx> ConstraintSet<'tcx> {
    pub fn new() -> ConstraintSet<'tcx> {
        ConstraintSet {
            less: BTreeSet::new(),
            greater: BTreeSet::new(),
        }
    }

    pub fn iter(&self) -> btree_set::Iter<(Perm<'tcx>, Perm<'tcx>)> {
        self.less.iter()
    }

    pub fn add(&mut self, a: Perm<'tcx>, b: Perm<'tcx>) {
        self.less.insert((a, b));
        self.greater.insert((b, a));
    }

    pub fn import(&mut self, other: &ConstraintSet<'tcx>) {
        eprintln!("IMPORT {} constraints", other.less.len());
        self.less.extend(other.less.iter().cloned().filter(|&(ref a, ref b)| {
            eprintln!("IMPORT CONSTRAINT: {:?} <= {:?}", a, b);
            true
        }));
        self.greater.extend(other.greater.iter().cloned());
    }

    pub fn import_substituted<F>(&mut self,
                                 other: &ConstraintSet<'tcx>,
                                 arena: &'tcx DroplessArena,
                                 mut f: F)
            where F: Fn(Perm<'tcx>) -> Perm<'tcx> {
        eprintln!("IMPORT {} constraints (substituted)", other.less.len());

        let subst_one = |p| {
            match p {
                Perm::Min(ps) => {
                    let mut buf = Vec::with_capacity(ps.len());
                    for &p in ps {
                        let q = f(p);
                        if !buf.contains(&q) {
                            buf.push(q);
                        }
                    }
                    Perm::Min(arena.alloc_slice(&buf))
                },
                p => f(p),
            }
        };

        for &(a, b) in other.less.iter() {
            let (a2, b2) = (subst_one(a), subst_one(b));
            eprintln!("IMPORT CONSTRANT: {:?} <= {:?} (substituted from {:?} <= {:?})",
                      a2, b2, a, b);
            self.add(a2, b2);
        }
    }

    pub fn lower_bound(&self, p: Perm<'tcx>) -> ConcretePerm {
        match p {
            Perm::Concrete(p) => return p,
            _ => {},
        }

        let mut seen = HashSet::new();
        let mut queue = VecDeque::new();
        let mut bound = ConcretePerm::Read;

        seen.insert(p);
        queue.push_back(p);

        while let Some(cur) = queue.pop_front() {
            for &(_, next) in self.greater.range(perm_range(cur)) {
                match next {
                    Perm::Concrete(p) => {
                        bound = cmp::max(bound, p);
                    },
                    _ => {
                        if !seen.contains(&next) {
                            seen.insert(next);
                            queue.push_back(next);
                        }
                    },
                }
            }
        }

        bound
    }

    pub fn upper_bound(&self, p: Perm<'tcx>) -> ConcretePerm {
        match p {
            Perm::Concrete(p) => return p,
            _ => {},
        }

        let mut seen = HashSet::new();
        let mut queue = VecDeque::new();
        let mut bound = ConcretePerm::Move;

        seen.insert(p);
        queue.push_back(p);

        while let Some(cur) = queue.pop_front() {
            for &(_, next) in self.less.range(perm_range(cur)) {
                match next {
                    Perm::Concrete(p) => {
                        bound = cmp::min(bound, p);
                    },
                    _ => {
                        if !seen.contains(&next) {
                            seen.insert(next);
                            queue.push_back(next);
                        }
                    },
                }
            }
        }

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
            where F: Fn(Perm<'tcx>) -> Option<ConcretePerm> {
        /// Evaluate a permission, recursing into `Perm::Min`s.  Returns the computed permission
        /// value along with two boolean flags `any_missing` and `all_missing`.
        fn eval_rec<'tcx, F>(p: Perm<'tcx>, eval: &F) -> (ConcretePerm, bool, bool)
                where F: Fn(Perm<'tcx>) -> Option<ConcretePerm> {
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
                },
                p => {
                    if let Some(c) = eval(p) {
                        (c, false, false)
                    } else {
                        (ConcretePerm::Move, true, true)
                    }
                },
            }
        }

        for &(a, b) in &self.less {
            let (a, a_any, a_all) = eval_rec(a, &eval);
            let (b, b_any, b_all) = eval_rec(b, &eval);

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

            return false;
        }

        true
    }

    pub fn edit<'a>(&'a mut self) -> EditConstraintSet<'a, 'tcx> {
        let to_visit = self.less.iter().cloned().collect();
        EditConstraintSet {
            cset: self,
            to_visit: to_visit,
        }
    }

    pub fn for_each_perm<F: FnMut(Perm<'tcx>)>(&self, mut f: F) {
        for &(a, b) in &self.less {
            a.for_each_atom(&mut f);
            b.for_each_atom(&mut f);
        }
    }
}

/// Helper for adding/removing constraints while also iterating over them.
pub struct EditConstraintSet<'a, 'tcx: 'a> {
    cset: &'a mut ConstraintSet<'tcx>,
    to_visit: VecDeque<(Perm<'tcx>, Perm<'tcx>)>,
}

impl<'a, 'tcx> EditConstraintSet<'a, 'tcx> {
    pub fn next(&mut self) -> Option<(Perm<'tcx>, Perm<'tcx>)> {
        while let Some((a, b)) = self.to_visit.pop_front() {
            if self.cset.less.contains(&(a, b)) {
                return Some((a, b));
            }
        }
        None
    }

    pub fn add(&mut self, a: Perm<'tcx>, b: Perm<'tcx>) {
        if self.cset.less.contains(&(a, b)) {
            return;
        }
        self.cset.less.insert((a, b));
        self.cset.greater.insert((b, a));
        self.to_visit.push_back((a, b));
    }

    pub fn add_no_visit(&mut self, a: Perm<'tcx>, b: Perm<'tcx>) {
        if self.cset.less.contains(&(a, b)) {
            return;
        }
        self.cset.less.insert((a, b));
        self.cset.greater.insert((b, a));
    }

    pub fn remove(&mut self, a: Perm<'tcx>, b: Perm<'tcx>) {
        self.cset.less.remove(&(a, b));
        self.cset.greater.remove(&(b, a));
        // If it remains in `to_visit`, it will be skipped by `next`.
    }
}


impl<'tcx> ConstraintSet<'tcx> {
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
                eprintln!("remove: {:?} <= {:?}", a, b);
                edit.remove(a, b);
            }
        }
    }

    pub fn expand_min_rhs(&mut self) {
        let mut edit = self.edit();

        while let Some((a, b)) = edit.next() {
            match b {
                Perm::Min(ps) => {
                    eprintln!("expand: {:?} <= {:?}", a, b);
                    edit.remove(a, b);
                    for &p in ps {
                        edit.add(a, p);
                    }
                },
                _ => {},
            }
        }
    }

    /// Simplify `min(...) <= ...` constraints as much as possible.
    pub fn simplify_min_lhs(&mut self, arena: &'tcx DroplessArena) {
        let mut edit = self.edit();

        'next: while let Some((a, b)) = edit.next() {
            let ps = match a {
                Perm::Min(ps) => ps,
                _ => continue,
            };

            if ps.len() == 0 {
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
                // we would end up with `min()`, becuase `a` eliminates `b` and `c`, `b` and
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
                    eprintln!("remove {:?} <= {:?} ({:?} <= {:?})", a, b, pi, b);
                    edit.remove(a, b);
                    continue 'next;
                }
            }

            assert!(to_remove.len() < ps.len(), "tried to remove all arguments of `min`");
            if to_remove.len() == ps.len() - 1 {
                // `min(p)` is the same as just `p`.
                edit.remove(a, b);
                let (_, p) = ps.iter().cloned().enumerate()
                    .filter(|&(i, _)| !to_remove.contains(&i)).next().unwrap();
                eprintln!("replace {:?} <= {:?} with {:?} <= {:?}", a, b, p, b);
                edit.add(p, b);
            } else if to_remove.len() > 0 {
                edit.remove(a, b);
                let ps = ps.iter().cloned().enumerate()
                    .filter(|&(i, _)| !to_remove.contains(&i))
                    .map(|(_, p)| p).collect::<Vec<_>>();
                let new_min = Perm::Min(arena.alloc_slice(&ps));
                eprintln!("replace {:?} <= {:?} with {:?} <= {:?}", a, b, new_min, b);
                edit.add(new_min, b);
            }
            // Otherwise, to_remove == 0, meaning we don't have any changes to apply.
        }
    }

    pub fn simplify(&mut self, arena: &'tcx DroplessArena) {
        self.remove_useless();
        self.expand_min_rhs();
        self.simplify_min_lhs(arena);
    }

    pub fn retain_perms<F>(&mut self, arena: &'tcx DroplessArena, filter: F)
            where F: Fn(Perm<'tcx>) -> bool {
        // Collect all atomic permissions that appear in the constraint set.
        let mut atomic_perms = HashSet::new();
        fn collect_atomic<'tcx>(p: Perm<'tcx>, dest: &mut HashSet<Perm<'tcx>>) {
            match p {
                Perm::Min(ps) => {
                    for &p in ps {
                        collect_atomic(p, dest);
                    }
                },
                _ => {
                    dest.insert(p);
                },
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

            eprintln!("removing perm {:?}", p);

            // Perms less than `p`, and perms greater than `p`.
            let less = self.greater.range(perm_range(p))
                .map(|&(a, b)| b).filter(|&b| b != p).collect::<Vec<_>>();
            let greater = self.less.range(perm_range(p))
                .map(|&(a, b)| b).filter(|&b| b != p).collect::<Vec<_>>();
            eprintln!("    less: {:?}", less);
            eprintln!("    greater: {:?}", greater);

            let mut edit = self.edit();
            while let Some((a, b)) = edit.next() {
                if !a.contains(p) && !b.contains(p) {
                    continue;
                }

                eprintln!("  remove {:?} <= {:?}", a, b);
                edit.remove(a, b);
                a.for_each_replacement(arena, p, &less, |a| {
                    b.for_each_replacement(arena, p, &greater, |b| {
                        eprintln!("    replacement: {:?} <= {:?}", a, b);
                        edit.add_no_visit(a, b);
                    });
                });
            }
        }
    }
}

