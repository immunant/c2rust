use std::fmt::{self, Display, Formatter};
use std::io::Write;

use env_logger::Env;
use log::{LevelFilter, Record};

struct DisplayRecord<'a>(&'a Record<'a>);

impl Display for DisplayRecord<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let level = self.0.level();
        let file = self.0.file().unwrap_or("?");
        let line = self.0.line().unwrap_or(0);
        let module_path = self.0.module_path().unwrap_or("?");
        let args = self.0.args();

        write!(f, "[{level} @ {file}:{line} @ {module_path}]: {args}")
    }
}

/// Initialize an [`env_logger::Logger`].
/// It behaves normally most of the time, being controlled by `$RUST_LOG`,
/// except `$RUST_LOG_PANIC` can also be set.
/// Anything that matches `$RUST_LOG_PANIC` will panic instead of being logged.
///
/// The defaults for these are:
/// * `RUST_LOG=debug`
/// * `RUST_LOG_PANIC=error`
///
/// so by default, `log::error!` panics,
/// but setting `RUST_LOG_PANIC=off` turns them into just being logged.
pub fn init_logger() {
    let log_env = Env::default().default_filter_or(LevelFilter::Debug.as_str());
    let panic_env = Env::default().filter_or("RUST_LOG_PANIC", LevelFilter::Error.as_str());

    let log_logger = env_logger::Builder::from_env(log_env).build();
    let panic_logger = env_logger::Builder::from_env(panic_env).build();

    // Create the actual [`Logger`] to log everything ([`LevelFilter::max()]`),
    // and then we can do the specific matching for `log_logger` and `panic_logger` inside the formatter,
    // as we don't want them to conflict.
    env_logger::builder()
        .filter_level(LevelFilter::max())
        .format(move |f, record| {
            let record = DisplayRecord(record);
            if log_logger.matches(record.0) {
                writeln!(f, "{record}")?;
            }
            if panic_logger.matches(record.0) {
                panic!("\n{record}\n");
            }
            Ok(())
        })
        .init();
}

#[test]
fn rust_log_doesnt_affect_panicking() {
    std::env::set_var("RUST_LOG", "c2rust_analyze::log=trace");
    init_logger();
    ::log::trace!("test");
}
