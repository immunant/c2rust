use itertools::Itertools;

use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::{
    cmp::min,
    collections::HashMap,
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
};

pub struct ShortOption<T>(pub Option<T>);

impl<T: Display> Display for ShortOption<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.0 {
            Some(this) => write!(f, "{}", this),
            None => write!(f, "_"),
        }
    }
}

impl<T: Debug> Debug for ShortOption<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.0 {
            Some(this) => write!(f, "{:?}", this),
            None => write!(f, "_"),
        }
    }
}

pub fn maybe_take<T>(iter: impl Iterator<Item = T>, n: Option<usize>) -> impl Iterator<Item = T> {
    iter.enumerate()
        .take_while(move |(i, _)| n.map(|n| *i < n).unwrap_or(true))
        .map(|(_, t)| t)
}

pub struct Duplicates<T> {
    duplicates: HashMap<T, Vec<T>>,
}

impl<T: Eq + Hash + Clone> Duplicates<T> {
    pub fn find(iter: impl IntoIterator<Item = T>) -> Self {
        let duplicates = iter
            .into_iter()
            .into_group_map_by(|e| e.clone())
            .into_iter()
            .filter(|(_, duplicates)| duplicates.len() > 1)
            .collect::<HashMap<_, _>>();
        Self { duplicates }
    }
}

impl<T> Duplicates<T> {
    pub fn is_empty(&self) -> bool {
        self.duplicates.is_empty()
    }

    pub fn len(&self) -> usize {
        self.duplicates.len()
    }
}

impl<T> Duplicates<T> {
    const DEFAULT_UP_TO_N: usize = 5;

    pub fn fmt_up_to_n(
        &self,
        f: &mut Formatter,
        n: Option<usize>,
        to_string: impl Fn(&T) -> String,
    ) -> fmt::Result {
        let num_remaining = |len: usize| {
            let num_printed = match n {
                Some(n) => min(n, len),
                None => len,
            };
            len - num_printed
        };

        writeln!(f, "{} duplicates:", self.len())?;
        for (_, duplicates) in maybe_take(self.duplicates.iter(), n) {
            let duplicates = duplicates
                .iter()
                .map(
                    // not redundant; otherwise would require a `Copy` bound
                    #[allow(clippy::redundant_closure)]
                    |t| to_string(t),
                )
                .counts()
                .into_iter()
                .map(|(duplicate, count)| (duplicate.split('\n').join("\n\t"), count))
                .map(|(duplicate, count)| match count {
                    0 => "".into(),
                    1 => duplicate,
                    _ => format!("({count}x) {duplicate}"),
                })
                .join(", ");
            writeln!(f, "\t{}", duplicates)?;
        }
        write!(f, "and {} more...", num_remaining(self.len()))?;
        Ok(())
    }
}

impl<T: Debug> Debug for Duplicates<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.fmt_up_to_n(f, Self::DEFAULT_UP_TO_N.into(), |t| format!("{t:?}"))
    }
}

impl<T: Display> Display for Duplicates<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.fmt_up_to_n(f, Self::DEFAULT_UP_TO_N.into(), |t| format!("{t}"))
    }
}

impl<T: Debug + Eq + Hash> Duplicates<T> {
    // Used for debugging.
    #[allow(dead_code)]
    pub fn assert_empty_debug(&self) {
        if self.is_empty() {
            return;
        }
        panic!("unexpected duplicates: {:?}", self);
    }
}

impl<T: Display + Eq + Hash> Duplicates<T> {
    // Used for debugging.
    #[allow(dead_code)]
    pub fn assert_empty_display(&self) {
        if self.is_empty() {
            return;
        }
        panic!("unexpected duplicates: {}", self);
    }
}

pub struct Padded<T> {
    to_pad: T,
    pad_len: usize,
}

impl<T: Display> Display for Padded<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}{:padding$}", self.to_pad, "", padding = self.pad_len)
    }
}

impl<T: Debug> Debug for Padded<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{:?}{:padding$}",
            self.to_pad,
            "",
            padding = self.pad_len
        )
    }
}

pub fn pad_columns(lines: &[String], split_sep: char, join_sep: &str) -> Vec<String> {
    let rows = lines
        .iter()
        .map(|line| line.split(split_sep).collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let num_columns = rows.iter().map(|line| line.len()).max().unwrap_or_default();
    let column_max_lengths = (0..num_columns)
        .map(|column| {
            rows.iter()
                .map(|cells| cells.get(column).copied().unwrap_or_default())
                .map(|cell| cell.len())
                .max()
                .unwrap_or_default()
        })
        .collect::<Vec<_>>();
    rows.iter()
        .map(|cells| {
            cells
                .iter()
                .zip(column_max_lengths.iter())
                .map(|(&cell, &max_length)| Padded {
                    to_pad: cell,
                    pad_len: max_length - cell.len(),
                })
                .join(join_sep)
        })
        .collect::<Vec<_>>()
}

pub enum HashSize {
    U32,
    U64,
}

impl Default for HashSize {
    fn default() -> Self {
        Self::U64
    }
}

/// Lazily [`Display`] the hash value of an `impl `[`Hash`] using [`DefaultHasher`].
pub struct DisplayHash<'a, T: Hash> {
    inner: &'a T,
    hash_size: HashSize,
}

impl<'a, T: Hash> DisplayHash<'a, T> {
    pub fn new(inner: &'a T, hash_size: HashSize) -> Self {
        Self { inner, hash_size }
    }

    pub fn to_u64(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.inner.hash(&mut hasher);
        hasher.finish()
    }

    pub fn to_u32(&self) -> u32 {
        let h = self.to_u64();
        let lo = h as u32;
        let hi = (h >> 32) as u32;
        lo ^ hi
    }

    pub fn u64_bytes(&self) -> [u8; 8] {
        self.to_u64().to_le_bytes()
    }

    pub fn u32_bytes(&self) -> [u8; 4] {
        self.to_u32().to_le_bytes()
    }

    pub fn base64(&self) -> String {
        match self.hash_size {
            HashSize::U32 => base64::encode(&self.u32_bytes()),
            HashSize::U64 => base64::encode(&self.u64_bytes()),
        }
    }

    pub fn base64_short(&self) -> String {
        self.base64().trim_end_matches('=').into()
    }
}

impl<T: Hash> Display for DisplayHash<'_, T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.base64_short())
    }
}
