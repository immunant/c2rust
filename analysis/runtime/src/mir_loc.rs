use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt::Debug;
use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Debug, Serialize, Deserialize, Clone, Hash, Eq, PartialEq)]
pub enum MirProjection {
    Deref,
    Field(usize),
    Index(usize),
    Unsupported,
}

impl Display for MirProjection {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use MirProjection::*;
        match self {
            Deref => write!(f, "*"),
            Field(i) => write!(f, "{i}"),
            Index(i) => write!(f, "[{i}]"),
            Unsupported => write!(f, "unsupported"),
        }
    }
}

/// See [`rustc_middle::mir::Local`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.Local.html).
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct Local {
    pub index: u32,
}

impl From<u32> for Local {
    fn from(index: u32) -> Self {
        Self { index }
    }
}

impl From<usize> for Local {
    fn from(index: usize) -> Self {
        let index = index.try_into().unwrap();
        Self { index }
    }
}

impl From<Local> for u32 {
    fn from(val: Local) -> Self {
        val.index
    }
}

impl From<Local> for usize {
    fn from(val: Local) -> Self {
        val.index.try_into().unwrap()
    }
}

impl Local {
    pub fn as_u32(&self) -> u32 {
        (*self).into()
    }

    pub fn as_usize(&self) -> usize {
        (*self).into()
    }
}

impl Debug for Local {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "_{}", self.index)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Serialize, Deserialize)]
pub struct MirPlace {
    pub local: Local,
    pub projection: Vec<MirProjection>,
}

impl Display for MirPlace {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self.local)?;
        for p in &self.projection {
            write!(f, ".{}", p)?;
        }
        Ok(())
    }
}

impl Debug for MirPlace {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub type MirLocId = u32;

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone, Debug)]
pub struct Fingerprint(pub u64, pub u64);

impl From<(u64, u64)> for Fingerprint {
    fn from(other: (u64, u64)) -> Self {
        Self(other.0, other.1)
    }
}

impl From<Fingerprint> for (u64, u64) {
    fn from(other: Fingerprint) -> Self {
        (other.0, other.1)
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone, Debug)]
pub struct DefPathHash(pub Fingerprint);

impl From<(u64, u64)> for DefPathHash {
    fn from(other: (u64, u64)) -> Self {
        Self(other.into())
    }
}

impl From<DefPathHash> for (u64, u64) {
    fn from(other: DefPathHash) -> Self {
        other.0.into()
    }
}

#[derive(Serialize, Deserialize, Eq, Clone)]
pub struct Func {
    pub def_path_hash: DefPathHash,
    pub name: String,
}

impl Func {
    fn cmp_fields(&self) -> impl Eq + Hash + Ord + '_ {
        self.def_path_hash
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.cmp_fields() == other.cmp_fields()
    }
}

impl Hash for Func {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.cmp_fields().hash(state);
    }
}

impl PartialOrd for Func {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cmp_fields().partial_cmp(&other.cmp_fields())
    }
}

impl Ord for Func {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_fields().cmp(&other.cmp_fields())
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Debug for Func {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TransferKind {
    None,
    Arg(DefPathHash),
    Ret(DefPathHash),
}

impl Default for TransferKind {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Hash, Clone, Default)]
pub struct EventMetadata {
    /// Input [`Local`]s for an [`Event`](crate::events::Event).
    pub source: Option<MirPlace>,
    /// Destination [`Local`] for an [`Event`](crate::events::Event).
    pub destination: Option<MirPlace>,
    /// Destination func [`DefPathHash`] of [`Event`](crate::events::Event).
    pub transfer_kind: TransferKind,
    /// Any string useful for debugging.
    pub debug_info: String,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct MirLoc {
    pub func: Func,
    pub basic_block_idx: usize,
    pub statement_idx: usize,
    pub metadata: EventMetadata,
}
