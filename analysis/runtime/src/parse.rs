//! Don't want a huge depedency on `clap`.

use std::{
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    marker::PhantomData,
};

pub trait AsStr {
    fn as_str(&self) -> &'static str;
}

pub trait Choices
where
    Self: Sized,
{
    fn choices() -> &'static [Self];
}

impl AsStr for bool {
    fn as_str(&self) -> &'static str {
        match self {
            true => "true",
            false => "false",
        }
    }
}

impl Choices for bool {
    fn choices() -> &'static [Self] {
        &[true, false]
    }
}

pub struct ChoiceError<T> {
    pub found: String,
    _phantom: PhantomData<T>,
}

impl<T: Choices + AsStr + 'static> Display for ChoiceError<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "found {}, but expected one of ", &self.found)?;
        f.debug_list()
            .entries(T::choices().iter().map(|choice| choice.as_str()));
        Ok(())
    }
}

impl<T: Choices + AsStr + 'static> Debug for ChoiceError<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl<T: Choices + AsStr + 'static> Error for ChoiceError<T> {}

pub fn one_of<T: Choices + AsStr + 'static>(s: String) -> Result<&'static T, ChoiceError<T>> {
    T::choices()
        .iter()
        .find(|choice| choice.as_str() == s.as_str())
        .ok_or(ChoiceError {
            found: s,
            _phantom: Default::default(),
        })
}
