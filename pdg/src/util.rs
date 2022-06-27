use itertools::Itertools;

use std::{
    cmp::min,
    collections::HashMap,
    fmt::{self, Debug, Formatter},
    hash::Hash,
};

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

impl<T: Debug> Duplicates<T> {
    pub fn fmt_up_to_n(&self, f: &mut Formatter<'_>, n: Option<usize>) -> fmt::Result {
        let num_remaining = |len: usize| {
            let num_printed = match n {
                Some(n) => min(n, len),
                None => len,
            };
            len - num_printed
        };

        write!(f, "{} duplicates:\n", self.len())?;
        for (_, duplicates) in maybe_take(self.duplicates.iter(), n) {
            let duplicates = duplicates
                .iter()
                .map(|e| format!("{:?}", e))
                .counts()
                .into_iter()
                .map(|(duplicate, count)| match count {
                    0 => "".into(),
                    1 => duplicate,
                    _ => format!("({count}x) {duplicate}"),
                })
                .join(", ");
            write!(f, "\t{}\n", duplicates)?;
        }
        write!(f, "and {} more...", num_remaining(self.len()))?;
        Ok(())
    }
}

impl<T: Debug> Debug for Duplicates<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_up_to_n(f, 10.into())
    }
}

impl<T: Debug + Eq + Hash> Duplicates<T> {
    pub fn assert_empty(&self) {
        if self.is_empty() {
            return;
        }
        panic!("unexpected duplicates: {:?}", self);
    }
}
