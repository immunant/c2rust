use std::fmt::{self, Display};
use std::io;
use syn;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Syn(syn::Error),
    Str(String),
    At(String, Box<Error>),
}

impl From<io::Error> for Error {
    fn from(x: io::Error) -> Error {
        Error::Io(x)
    }
}

impl From<syn::Error> for Error {
    fn from(x: syn::Error) -> Error {
        Error::Syn(x)
    }
}

impl From<String> for Error {
    fn from(x: String) -> Error {
        Error::Str(x)
    }
}

impl From<&str> for Error {
    fn from(x: &str) -> Error {
        Error::Str(x.to_owned())
    }
}

impl Error {
    pub fn at(self, desc: impl Display) -> Error {
        Error::At(desc.to_string(), Box::new(self))
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Io(x) => Display::fmt(x, f),
            Error::Syn(x) => Display::fmt(x, f),
            Error::Str(x) => Display::fmt(x, f),
            Error::At(desc, inner) => write!(f, "{desc}: {inner}"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Io(x) => Some(x),
            Error::Syn(x) => Some(x),
            Error::Str(_) => None,
            Error::At(_, x) => Some(x),
        }
    }
}
