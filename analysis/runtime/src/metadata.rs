use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};

use serde::{Deserialize, Serialize};

use crate::mir_loc::{DefPathHash, MirLoc, MirLocId};

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

        pub trait IWithMetadata
        where
            Self: Sized,
        {
            fn with_metadata<'a>(&'a self, metadata: &'a Metadata) -> WithMetadata<'a, Self> {
                WithMetadata {
                    inner: self,
                    metadata,
                }
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
