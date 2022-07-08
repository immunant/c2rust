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

impl Debug for MirLoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let MirLoc {
            body_def: _,
            basic_block_idx,
            statement_idx,
            metadata: _,
            fn_name,
        } = self;
        write!(f, "{fn_name}:{basic_block_idx}:{statement_idx}")
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
        let locs = DebugFromFn(|f| {
            let locs = self.locs.iter().map(|loc| loc);
            f.debug_list().entries(locs).finish()
        });
        let functions = DebugFromFn(|f| {
            let functions = self
                .functions
                .iter()
                .map(|(def_path_hash, func)| (def_path_hash, func));
            f.debug_map().entries(functions).finish()
        });
        f.debug_struct("Metadata")
            .field("locs", &locs)
            .field("functions", &functions)
            .finish()
    }
}
