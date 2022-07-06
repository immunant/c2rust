use c2rust_analysis_rt::mir_loc::DebugFromFn;
use itertools::Itertools;

use std::{
    cmp::min,
    collections::HashMap,
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
};

pub struct ShortOption<T>(pub Option<T>);

impl<T: Display> Display for ShortOption<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Some(this) => write!(f, "{}", this),
            None => write!(f, "_"),
        }
    }
}

impl<T: Debug> Debug for ShortOption<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
        f: &mut Formatter<'_>,
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_up_to_n(f, Self::DEFAULT_UP_TO_N.into(), |t| format!("{t:?}"))
    }
}

impl<T: Display> Display for Duplicates<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_up_to_n(f, Self::DEFAULT_UP_TO_N.into(), |t| format!("{t}"))
    }
}

impl<T> Duplicates<T> {
    // Used for debugging.
    #[allow(dead_code)]
    pub fn assert_empty_with(&self, to_string: impl Copy + Fn(&T) -> String) {
        if self.is_empty() {
            return;
        }
        panic!("unexpected duplicates: {:?}", DebugFromFn(|f| {
            self.fmt_up_to_n(f, Self::DEFAULT_UP_TO_N.into(), to_string)
        }));
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
