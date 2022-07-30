use polonius_engine::{self, Atom, FactTypes};
use rustc_middle::mir::{BasicBlock, Local, Location, Place, PlaceElem};
use rustc_middle::ty::TyCtxt;
use std::collections::hash_map::{Entry, HashMap};
use std::hash::Hash;

macro_rules! define_atom_type {
    ($Atom:ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
        pub struct $Atom(usize);

        impl From<usize> for $Atom {
            fn from(x: usize) -> $Atom {
                $Atom(x)
            }
        }

        impl From<$Atom> for usize {
            fn from(x: $Atom) -> usize {
                x.0
            }
        }

        impl Atom for $Atom {
            fn index(self) -> usize {
                self.0
            }
        }
    };
}

define_atom_type!(Origin);
define_atom_type!(Loan);
define_atom_type!(Point);
define_atom_type!(Variable);
define_atom_type!(Path);

#[derive(Clone, Copy, Debug, Default)]
pub struct AnalysisFactTypes;
impl FactTypes for AnalysisFactTypes {
    type Origin = Origin;
    type Loan = Loan;
    type Point = Point;
    type Variable = Variable;
    type Path = Path;
}

pub type AllFacts = polonius_engine::AllFacts<AnalysisFactTypes>;
pub type Output = polonius_engine::Output<AnalysisFactTypes>;

#[derive(Clone, Debug)]
struct AtomMap<T, A> {
    atom_to_thing: Vec<T>,
    thing_to_atom: HashMap<T, A>,
}

impl<T, A> Default for AtomMap<T, A> {
    fn default() -> AtomMap<T, A> {
        AtomMap {
            atom_to_thing: Vec::new(),
            thing_to_atom: HashMap::new(),
        }
    }
}

impl<T: Hash + Eq + Clone, A: Atom> AtomMap<T, A> {
    #[allow(dead_code)]
    pub fn new() -> AtomMap<T, A> {
        AtomMap {
            atom_to_thing: Vec::new(),
            thing_to_atom: HashMap::new(),
        }
    }

    pub fn add(&mut self, x: T) -> A {
        match self.thing_to_atom.entry(x.clone()) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let atom = A::from(self.atom_to_thing.len());
                self.atom_to_thing.push(x);
                e.insert(atom);
                atom
            }
        }
    }

    pub fn add_new(&mut self, x: T) -> (A, bool) {
        match self.thing_to_atom.entry(x.clone()) {
            Entry::Occupied(e) => (*e.get(), false),
            Entry::Vacant(e) => {
                let atom = A::from(self.atom_to_thing.len());
                self.atom_to_thing.push(x);
                e.insert(atom);
                (atom, true)
            }
        }
    }

    pub fn get(&self, x: A) -> T {
        self.atom_to_thing[x.into()].clone()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum SubPoint {
    Start,
    Mid,
}

#[derive(Clone, Debug, Default)]
pub struct AtomMaps<'tcx> {
    next_origin: usize,
    next_loan: usize,
    point: AtomMap<(BasicBlock, usize, SubPoint), Point>,
    path: AtomMap<(Local, &'tcx [PlaceElem<'tcx>]), Path>,
}

impl<'tcx> AtomMaps<'tcx> {
    pub fn origin(&mut self) -> Origin {
        let idx = self.next_origin;
        self.next_origin += 1;
        Origin(idx)
    }

    pub fn loan(&mut self) -> Loan {
        let idx = self.next_loan;
        self.next_loan += 1;
        Loan(idx)
    }

    pub fn point(&mut self, bb: BasicBlock, idx: usize, sub: SubPoint) -> Point {
        self.point.add((bb, idx, sub))
    }

    pub fn point_mid_location(&mut self, loc: Location) -> Point {
        self.point(loc.block, loc.statement_index, SubPoint::Mid)
    }

    pub fn get_point(&self, x: Point) -> (BasicBlock, usize, SubPoint) {
        self.point.get(x)
    }

    pub fn get_point_location(&self, x: Point) -> Location {
        let (block, statement_index, _) = self.get_point(x);
        Location {
            block,
            statement_index,
        }
    }

    pub fn variable(&mut self, l: Local) -> Variable {
        Variable(l.as_usize())
    }

    pub fn _get_variable(&self, x: Variable) -> Local {
        Local::from_usize(x.0)
    }

    pub fn path(&mut self, facts: &mut AllFacts, place: Place<'tcx>) -> Path {
        self.path_slice(facts, place.local, place.projection)
    }

    fn path_slice(
        &mut self,
        facts: &mut AllFacts,
        local: Local,
        projection: &'tcx [PlaceElem<'tcx>],
    ) -> Path {
        let (path, new) = self.path.add_new((local, projection));
        if new {
            if projection.is_empty() {
                let var = self.variable(local);
                facts.path_is_var.push((path, var));
            } else {
                let parent = self.path_slice(facts, local, &projection[..projection.len() - 1]);
                // TODO: check ordering of arguments here
                facts.child_path.push((parent, path));
            }
        }
        path
    }

    pub fn _get_path(&self, tcx: TyCtxt<'tcx>, x: Path) -> Place<'tcx> {
        let (local, projection) = self.path.get(x);
        let projection = tcx.intern_place_elems(projection);
        Place { local, projection }
    }

    pub fn get_path_projection(&self, _tcx: TyCtxt<'tcx>, x: Path) -> &'tcx [PlaceElem<'tcx>] {
        let (_local, projection) = self.path.get(x);
        projection
    }
}
