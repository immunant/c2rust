use colored::Colorize;
use failure::{err_msg, Backtrace, Context, Error, Fail};
use fern::colors::ColoredLevelConfig;
use log::Level;
use std::collections::HashSet;
use std::fmt::{self, Display};
use std::io;
use std::str::FromStr;
use std::sync::Arc;

use crate::c_ast::DisplaySrcSpan;
use c2rust_ast_exporter::get_clang_major_version;

const DEFAULT_WARNINGS: &[Diagnostic] = &[];

#[derive(PartialEq, Eq, Hash, Debug, Display, EnumString, Clone)]
#[strum(serialize_all = "kebab_case")]
pub enum Diagnostic {
    All,
    Comments,
}

#[allow(unused_macros)]
macro_rules! diag {
    ($type:path, $($arg:tt)*) => (warn!(target: &$type.to_string(), $($arg)*))
}

pub fn init(mut enabled_warnings: HashSet<Diagnostic>, log_level: log::LevelFilter) {
    enabled_warnings.extend(DEFAULT_WARNINGS.iter().cloned());

    let colors = ColoredLevelConfig::new();
    fern::Dispatch::new()
        .format(move |out, message, record| {
            let level_label = match record.level() {
                Level::Error => "error",
                Level::Warn => "warning",
                Level::Info => "info",
                Level::Debug => "debug",
                Level::Trace => "trace",
            };
            let target = record.target();
            let warn_flag = if let Ok(_) = Diagnostic::from_str(target) {
                format!(" [-W{}]", target)
            } else {
                String::new()
            };
            out.finish(format_args!(
                "\x1B[{}m{}:\x1B[0m {}{}",
                colors.get_color(&record.level()).to_fg_str(),
                level_label,
                message,
                warn_flag,
            ))
        })
        .level(log_level)
        .filter(move |metadata| {
            if enabled_warnings.contains(&Diagnostic::All) {
                return true;
            }
            Diagnostic::from_str(metadata.target())
                .map(|d| enabled_warnings.contains(&d))
                .unwrap_or(true)
        })
        .chain(io::stderr())
        .apply()
        .expect("Could not set up diagnostics");
}


#[derive(Debug, Clone)]
pub struct TranslationError {
    loc: Vec<DisplaySrcSpan>,
    inner: Arc<Context<TranslationErrorKind>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TranslationErrorKind {
    Generic,

    // Not enough simd intrinsics are available in LLVM < 7
    OldLLVMSimd,

    // We are waiting for va_copy support to land in rustc
    VaCopyNotImplemented,
}

/// Constructs a `TranslationError` using the standard string interpolation syntax.
#[macro_export]
macro_rules! format_translation_err {
    ($loc:expr, $($arg:tt)*) => {
        TranslationError::new(
            $loc,
            failure::err_msg(format!($($arg)*))
                .context(TranslationErrorKind::Generic),
        )
    }
}

impl Display for TranslationErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TranslationErrorKind::*;
        match self {
            Generic => {}

            OldLLVMSimd => {
                if let Some(version) = get_clang_major_version() {
                    if version < 7 {
                        return write!(f, "SIMD intrinsics require LLVM 7 or newer. Please build C2Rust against a newer LLVM version.");
                    }
                }
            }

            VaCopyNotImplemented => {
                return write!(f, "Rust does not yet support a C-compatible va_copy which is required to translate this function. See https://github.com/rust-lang/rust/pull/59625");
            }
        }
        Ok(())
    }
}

impl Fail for TranslationError {
    fn cause(&self) -> Option<&dyn Fail> {
        self.inner.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.inner.backtrace()
    }
}

impl Display for TranslationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(cause) = self.cause() {
            writeln!(f, "{}", cause)?;
        }
        match self.inner.get_context() {
            TranslationErrorKind::Generic => {}
            ref kind => writeln!(f, "{}", kind)?,
        }
        for loc in &self.loc {
            writeln!(f, "{} {}", "-->".blue(), loc)?;
        }
        Ok(())
    }
}

impl TranslationError {
    pub fn kind(&self) -> TranslationErrorKind {
        self.inner.get_context().clone()
    }

    pub fn new(loc: Option<DisplaySrcSpan>, inner: Context<TranslationErrorKind>) -> Self {
        let mut loc_stack = vec![];
        if let Some(loc) = loc {
            loc_stack.push(loc.clone());
        }
        TranslationError {
            loc: loc_stack,
            inner: Arc::new(inner),
        }
    }

    pub fn generic(msg: &'static str) -> Self {
        TranslationError {
            loc: vec![],
            inner: Arc::new(err_msg(msg).context(TranslationErrorKind::Generic)),
        }
    }

    pub fn add_loc(mut self, loc: Option<DisplaySrcSpan>) -> Self {
        if let Some(loc) = loc {
            self.loc.push(loc);
        }
        self
    }
}

impl From<&'static str> for TranslationError {
    fn from(msg: &'static str) -> TranslationError {
        TranslationError {
            loc: vec![],
            inner: Arc::new(err_msg(msg).context(TranslationErrorKind::Generic)),
        }
    }
}

impl From<Error> for TranslationError {
    fn from(e: Error) -> TranslationError {
        TranslationError {
            loc: vec![],
            inner: Arc::new(e.context(TranslationErrorKind::Generic)),
        }
    }
}

impl From<TranslationErrorKind> for TranslationError {
    fn from(kind: TranslationErrorKind) -> TranslationError {
        TranslationError {
            loc: vec![],
            inner: Arc::new(Context::new(kind)),
        }
    }
}

impl From<Context<TranslationErrorKind>> for TranslationError {
    fn from(ctx: Context<TranslationErrorKind>) -> TranslationError {
        TranslationError {
            loc: vec![],
            inner: Arc::new(ctx),
        }
    }
}
