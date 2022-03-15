use std::collections::hash_map::{HashMap, Entry};
use std::hash::Hash;
use polonius_engine::{self, Atom, FactTypes};
use rustc_middle::mir::{BasicBlock, Local, Location};

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
            fn index(self) -> usize { self.0 }
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
    pub fn new() -> AtomMap<T, A> {
        AtomMap {
            atom_to_thing: Vec::new(),
            thing_to_atom: HashMap::new(),
        }
    }

    pub fn add(&mut self, x: T) -> A {
        match self.thing_to_atom.entry(x.clone()) {
            Entry::Occupied(e) => {
                *e.get()
            },
            Entry::Vacant(e) => {
                let atom = A::from(self.atom_to_thing.len());
                self.atom_to_thing.push(x);
                e.insert(atom);
                atom
            },
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
pub struct AtomMaps {
    point: AtomMap<(BasicBlock, usize, SubPoint), Point>,
}

impl AtomMaps {
    pub fn point(&mut self, bb: BasicBlock, idx: usize, sub: SubPoint) -> Point {
        self.point.add((bb, idx, sub))
    }

    pub fn point_mid_location(&mut self, loc: Location) -> Point {
        self.point(loc.block, loc.statement_index, SubPoint::Mid)
    }

    pub fn get_point(&self, x: Point) -> (BasicBlock, usize, SubPoint) {
        self.point.get(x)
    }

    pub fn variable(&mut self, l: Local) -> Variable {
        Variable(l.as_usize())
    }

    pub fn get_variable(&self, x: Variable) -> Local {
        Local::from_usize(x.0)
    }
}


