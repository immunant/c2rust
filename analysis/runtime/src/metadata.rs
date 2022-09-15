use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
    io::Cursor,
    iter,
};

use serde::{de::DeserializeOwned, Deserialize, Serialize};

use crate::mir_loc::{DefPathHash, Func, MirLoc, MirLocId};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Metadata {
    pub locs: Vec<MirLoc>,
    pub functions: HashMap<DefPathHash, String>,
}

impl Metadata {
    pub fn get(&self, index: MirLocId) -> &MirLoc {
        &self.locs[index as usize]
    }

    pub fn read(bytes: &[u8]) -> bincode::Result<Self> {
        bincode_deserialize_many(bytes)
    }
}

fn bincode_deserialize_many<T, C>(bytes: &[u8]) -> bincode::Result<C>
where
    T: DeserializeOwned,
    C: FromIterator<T>,
{
    let len = bytes.len();
    let mut cursor = Cursor::new(bytes);
    iter::from_fn(|| {
        // No good alternatives: <https://github.com/rust-lang/rust/issues/86369>.
        if cursor.position() == len.try_into().unwrap() {
            return None;
        }
        Some(bincode::deserialize_from(&mut cursor))
    })
    .collect::<Result<_, _>>()
}

impl FromIterator<Metadata> for Metadata {
    fn from_iter<I: IntoIterator<Item = Metadata>>(iter: I) -> Self {
        let mut locs = Vec::new();
        let mut functions = HashMap::new();
        for metadata in iter {
            locs.extend(metadata.locs);
            functions.extend(metadata.functions);
        }
        Self { locs, functions }
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
