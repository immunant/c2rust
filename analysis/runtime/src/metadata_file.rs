use std::{
    io::{self, Read, Seek, Write},
    path::Path,
};

use fs_err::{File, OpenOptions};

use crate::metadata::Metadata;

/// An opened [`Metadata`] [`File`].
pub struct MetadataFile {
    file: File,
    bytes: Vec<u8>,
    metadata: Option<Metadata>,
}

impl MetadataFile {
    pub fn open(path: &Path) -> io::Result<Self> {
        let mut file = OpenOptions::new()
            .create(true)
            .truncate(false)
            .read(true)
            .write(true)
            .open(path)?;
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)?;
        let metadata = match bytes.as_slice() {
            &[] => None,
            bytes => bincode::deserialize(bytes).map_err(|e| {
                // Ignore (log) the deserialization error.
                // We can just regenerate the [`Metadata`],
                // as it was likely in an old binary format.
                eprintln!("metadata deserialization error; metadata format likely out-of-date; regenerating and overwriting: {e}");
            }).ok(),
        };
        Ok(Self {
            file,
            bytes,
            metadata,
        })
    }

    pub fn has_existing_metadata(&self) -> bool {
        self.metadata.is_some()
    }

    pub fn update(&mut self, updates: Metadata) -> io::Result<()> {
        match self.metadata.as_mut() {
            Some(metadata) => {
                metadata.update(updates);
            }
            None => {
                self.metadata = Some(updates);
            }
        };
        self.bytes.clear();
        bincode::serialize_into(&mut self.bytes, &self.metadata)
            .expect("error shouldn't occur in `bincode::serialize_into` a `Vec`");
        self.file.rewind()?;
        self.file.write_all(&self.bytes)?;
        // Do this unconditionally (i.e. don't skip when `self.bytes.len() > ` the old `self.bytes.len()`)
        // in case there are any concurrent accesses to the same file.
        // Maybe we should just use a proper file lock, though.
        self.file.set_len(self.bytes.len().try_into().unwrap())?;
        Ok(())
    }
}
