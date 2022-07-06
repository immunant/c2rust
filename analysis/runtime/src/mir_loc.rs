use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Serialize, Deserialize, Clone, Hash, Eq, PartialEq)]
pub enum MirProjection {
    Deref,
    Field(usize),
    Index(usize),
    Unsupported,
}

/// See [`rustc_middle::mir::Local`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.Local.html).
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct Local {
    /// TODO(kkysen) change to u32 like
    /// [`rustc_middle::mir::Local`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.Local.html),
    /// but need to keep bincode binary format.
    pub index: usize,
}

impl From<u32> for Local {
    fn from(index: u32) -> Self {
        Self {
            index: index.try_into().unwrap(),
        }
    }
}

impl From<usize> for Local {
    fn from(index: usize) -> Self {
        // Want it to work w/o code changes if I change the underlying type to `u32`.
        #[allow(clippy::useless_conversion)]
        let index = index.try_into().unwrap();
        Self { index }
    }
}

impl From<Local> for u32 {
    fn from(val: Local) -> Self {
        val.index.try_into().unwrap()
    }
}

impl From<Local> for usize {
    fn from(val: Local) -> Self {
        // Want it to work w/o code changes if I change the underlying type to `u32`.
        #[allow(clippy::useless_conversion)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "_{}", self.index)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Serialize, Deserialize)]
pub struct MirPlace {
    pub local: Local,
    pub projection: Vec<MirProjection>,
}

impl Display for MirPlace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.local)?;
        for p in &self.projection {
            write!(f, ".{:?}", p)?;
        }
        Ok(())
    }
}

impl Debug for MirPlace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub type MirLocId = u32;

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Clone)]
pub struct DefPathHash(pub u64, pub u64);

impl From<(u64, u64)> for DefPathHash {
    fn from(other: (u64, u64)) -> Self {
        Self(other.0, other.1)
    }
}

impl From<DefPathHash> for (u64, u64) {
    fn from(other: DefPathHash) -> Self {
        (other.0, other.1)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TransferKind {
    None,
    Arg((u64, u64)),
    Ret((u64, u64)),
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Hash, Clone)]
pub struct EventMetadata {
    // input Locals for an event
    pub source: Option<MirPlace>,
    // destination Local for an event
    pub destination: Option<MirPlace>,
    // destination func DefPathHash of event
    pub transfer_kind: TransferKind,
}

impl<'tcx> Default for EventMetadata {
    fn default() -> Self {
        Self {
            source: None,
            destination: None,
            transfer_kind: TransferKind::None,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct MirLoc {
    pub body_def: DefPathHash,
    pub basic_block_idx: usize,
    pub statement_idx: usize,
    pub metadata: EventMetadata,
}

#[derive(Serialize, Deserialize)]
pub struct Metadata {
    pub locs: Vec<MirLoc>,
    pub functions: HashMap<DefPathHash, String>,
}

impl Metadata {
    pub fn get(&self, index: MirLocId) -> &MirLoc {
        &self.locs[index as usize]
    }
}

/// Implemented as a macro because of the orphan rule.
/// It's a simple implementation so it's easiest just to copy and paste it with a macro.
#[macro_export]
macro_rules! decl_with_metadata {
    () => {
        pub struct WithMetadata<'a, T> {
            pub inner: &'a T,
            pub metadata: &'a Metadata,
        }
        
        pub trait IWithMetadata where Self: Sized {
            fn with_metadata<'a>(&'a self, metadata: &'a Metadata) -> WithMetadata<'a, Self> {
                WithMetadata { inner: self, metadata }
            }
        }
    };
}

decl_with_metadata!();

impl IWithMetadata for DefPathHash {}

impl Debug for WithMetadata<'_, DefPathHash> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let func_name = self
            .metadata
            .functions
            .get(self.inner)
            .map(|s| s.as_str())
            .unwrap_or("unknown");
        write!(f, "{func_name}")
    }
}

impl IWithMetadata for MirLoc {}

impl Debug for WithMetadata<'_, MirLoc> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let MirLoc {
            body_def,
            basic_block_idx,
            statement_idx,
            metadata: _,
        } = self.inner;
        let body_def = body_def.with_metadata(self.metadata);
        write!(f, "{body_def:?}:{basic_block_idx}:{statement_idx}")
    }
}

pub struct DebugFromFn<F>(pub F)
where
    F: Copy + FnOnce(&mut Formatter) -> fmt::Result;

impl<F> Debug for DebugFromFn<F>
where
    F: Copy + FnOnce(&mut Formatter) -> fmt::Result,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0(f)
    }
}

impl Debug for Metadata {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // f.debug_map().entries(entries)
        let locs = DebugFromFn(|f| {
            let locs = self.locs.iter().map(|loc| loc.with_metadata(self));
            f.debug_list().entries(locs).finish()
        });
        let functions = DebugFromFn(|f| {
            let functions = self
                .functions
                .iter()
                .map(|(def_path_hash, func)| (def_path_hash.with_metadata(self), func));
            f.debug_map().entries(functions).finish()
        });
        f.debug_struct("Metadata")
            .field("locs", &locs)
            .field("functions", &functions)
            .finish()
    }
}
