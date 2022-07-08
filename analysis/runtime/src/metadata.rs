use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};

use serde::{Deserialize, Serialize};

use crate::mir_loc::{DefPathHash, Func, MirLoc, MirLocId};

#[derive(Debug, Serialize, Deserialize)]
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
            func:
                Func {
                    def_path_hash: _,
                    name: fn_name,
                },
            basic_block_idx,
            statement_idx,
            metadata: _,
        } = self;
        write!(f, "{fn_name}:{basic_block_idx}:{statement_idx}")
    }
}
