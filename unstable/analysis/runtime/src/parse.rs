//! Don't want a huge dependency on `clap`.

use std::{
    ffi::OsStr,
    fmt::{self, Debug, Display, Formatter},
    marker::PhantomData,
};

pub trait AsStr {
    fn as_str(&self) -> &'static str;
}

pub struct Choices<T> {
    _phantom: PhantomData<T>,
}

impl<T> Default for Choices<T> {
    fn default() -> Self {
        Self {
            _phantom: Default::default(),
        }
    }
}

pub trait GetChoices
where
    Self: Sized,
{
    fn choices() -> &'static [Self];
}

impl<T: GetChoices + AsStr + 'static> Debug for Choices<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("one of ")?;
        f.debug_list()
            .entries(T::choices().iter().map(|choice| choice.as_str()))
            .finish()?;
        Ok(())
    }
}

impl<T: GetChoices + AsStr + 'static> Display for Choices<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl AsStr for bool {
    fn as_str(&self) -> &'static str {
        match self {
            true => "true",
            false => "false",
        }
    }
}

impl GetChoices for bool {
    fn choices() -> &'static [Self] {
        &[true, false]
    }
}

pub fn one_of<T: GetChoices + AsStr + 'static>(s: &OsStr) -> Result<&'static T, String> {
    T::choices()
        .iter()
        .find(|choice| s == OsStr::new(choice.as_str()))
        .ok_or_else(|| {
            let s = s.to_string_lossy();
            let choices = Choices::<T>::default();
            format!("found \"{s}\", but expected {choices}")
        })
}

pub mod env {
    use std::{env, ffi::OsStr, path::PathBuf};

    use super::{AsStr, Choices, GetChoices};

    pub fn path<K: AsRef<OsStr>>(var: K) -> Result<PathBuf, String> {
        let path = env::var_os(var.as_ref()).ok_or_else(|| {
            let var = var.as_ref().to_string_lossy().into_owned();
            format!("missing ${var}, must be a path")
        })?;
        Ok(path.into())
    }

    pub fn one_of<K: AsRef<OsStr>, T: GetChoices + AsStr + 'static>(
        var: K,
    ) -> Result<&'static T, String> {
        let value = env::var_os(var.as_ref()).ok_or_else(|| {
            let var = var.as_ref().to_string_lossy();
            let choices = Choices::<T>::default();
            format!("missing ${var}, must be {choices}")
        })?;
        super::one_of(&value)
    }
}
