use serde::{Deserialize, Serialize};

use rustc_middle::mir::{BasicBlock, Local};
use rustc_target::abi::FieldIdx;

#[derive(Serialize, Deserialize)]
#[serde(remote = "FieldIdx")]
pub struct FieldDef {
    #[serde(getter = "field_as_u32")]
    raw: u32,
}

fn field_as_u32(f: &FieldIdx) -> u32 {
    f.as_u32()
}

impl From<FieldDef> for FieldIdx {
    fn from(def: FieldDef) -> FieldIdx {
        FieldIdx::from_u32(def.raw)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "Local")]
pub struct LocalDef {
    #[serde(getter = "local_as_u32")]
    raw: u32,
}

fn local_as_u32(f: &Local) -> u32 {
    f.as_u32()
}

impl From<LocalDef> for Local {
    fn from(def: LocalDef) -> Local {
        Local::from_u32(def.raw)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "BasicBlock")]
pub struct BasicBlockDef {
    #[serde(getter = "basic_block_as_u32")]
    raw: u32,
}

fn basic_block_as_u32(f: &BasicBlock) -> u32 {
    f.as_u32()
}

impl From<BasicBlockDef> for BasicBlock {
    fn from(def: BasicBlockDef) -> BasicBlock {
        BasicBlock::from_u32(def.raw)
    }
}

pub mod index_vec {
    use rustc_index::vec::{Idx, IndexVec};
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn serialize<S, I, T>(iv: &IndexVec<I, T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        I: Idx,
        T: Serialize,
    {
        iv.iter().as_slice().serialize(serializer)
    }

    pub fn deserialize<'de, D, I, T>(deserializer: D) -> Result<IndexVec<I, T>, D::Error>
    where
        D: Deserializer<'de>,
        I: Idx,
        T: Deserialize<'de>,
    {
        let raw = Vec::<T>::deserialize(deserializer)?;
        Ok(IndexVec::from_raw(raw))
    }
}
