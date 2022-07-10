use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug, Formatter},
};

use serde::{Deserialize, Serialize};

use crate::mir_loc::{DefPathHash, Func, MirLoc, MirLocId};

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Metadata {
    pub locs: Vec<MirLoc>,
    pub functions: HashMap<DefPathHash, String>,
}

impl Metadata {
    pub fn get(&self, index: MirLocId) -> &MirLoc {
        &self.locs[index as usize]
    }

    pub fn update(&mut self, updates: Self) {
        let old_locs = self.locs.iter().collect::<HashSet<_>>();
        let new_locs = updates
            .locs
            .into_iter()
            .filter(|loc| !old_locs.contains(loc))
            // `.drain_filter()` would be better but it's unstable
            .collect::<Vec<_>>();
        self.locs.extend(new_locs);
        self.functions.extend(updates.functions);
    }
}

impl Debug for MirLoc {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
