use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::path::PathBuf;
use std::sync::RwLock;

lazy_static! {
    static ref MIR_LOC_FILE_PATH: RwLock<Option<PathBuf>> = RwLock::new(None);
}

pub fn set_file(file_path: &str) {
    *MIR_LOC_FILE_PATH.write().unwrap() = Some(PathBuf::from(file_path));
}

lazy_static! {
    pub(crate) static ref MIR_LOCS: Metadata = {
        let path = MIR_LOC_FILE_PATH
            .read()
            .expect("MIR_LOC_FILE_PATH was locked")
            .clone()
            .expect("MIR_LOC_FILE_PATH not initialized by the instrumented code");
        let file =
            File::open(&path).expect(&format!("Could not open span file: {:?}", path.to_str()));
        bincode::deserialize_from(file).expect("Error deserializing span file")
    };
}

pub fn get(index: MirLocId) -> Option<&'static MirLoc> {
    if MIR_LOC_FILE_PATH.read().unwrap().is_some() {
        Some(&MIR_LOCS.locs[index as usize])
    } else {
        None
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Hash, Eq, PartialEq)]
pub enum MirProjection {
    Deref,
    Field(usize),
    Index(usize),
    Unsupported
}

#[derive(Eq, PartialEq, Hash, Clone, Serialize, Deserialize)]
pub struct MirPlace {
    pub local: usize,
    pub projection: Vec<MirProjection>,
}

impl fmt::Debug for MirPlace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_{}", self.local)?;
        for p in &self.projection {
            write!(f, ".{:?}", p)?;
        }
        Ok(())
    }
}

pub type MirLocId = u32;

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Clone)]
pub struct DefPathHash(pub u64, pub u64);

impl fmt::Debug for DefPathHash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let entry = "unknown".to_string();
        write!(f, "{}", MIR_LOCS.functions.get(self).unwrap_or(&entry))
    }
}

impl From<(u64, u64)> for DefPathHash {
    fn from(other: (u64, u64)) -> Self {
        Self(other.0, other.1)
    }
}

impl Into<(u64, u64)> for DefPathHash {
    fn into(self) -> (u64, u64) {
        (self.0, self.1)
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

impl<'tcx> fmt::Debug for MirLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:?}:{}:{}",
            self.body_def, self.basic_block_idx, self.statement_idx
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Metadata {
    pub locs: Vec<MirLoc>,
    pub functions: HashMap<DefPathHash, String>,
}
