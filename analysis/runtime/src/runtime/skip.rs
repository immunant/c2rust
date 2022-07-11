use std::{
    fmt::{self, Display, Formatter},
    sync::atomic::{AtomicU64, Ordering},
};

use once_cell::sync::OnceCell;

use crate::events::Event;

#[derive(Debug)]
pub enum SkipReason {
    BeforeMain,
    AfterMain,
}

impl Display for SkipReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let (before_or_after, array, tors) = match self {
            Self::BeforeMain => ("before", "init_array", "ctors"),
            Self::AfterMain => ("after", "fini_array", "dtors"),
        };
        write!(f, "events {before_or_after} `main`, likely due to static initializers run in the `.{array}` or `.{tors}` sections")
    }
}

static EVENTS_SKIPPED_BEFORE_MAIN: AtomicU64 = AtomicU64::new(0);

static WARNED_AFTER_MAIN: OnceCell<()> = OnceCell::new();

/// Notify the user if any [`Event`]s were skipped before `main`.
///
/// # Safety
/// Must not be called before `main`.
pub fn notify_if_events_were_skipped_before_main() {
    let n = EVENTS_SKIPPED_BEFORE_MAIN.load(Ordering::Relaxed);
    let reason = SkipReason::BeforeMain;
    if n > 0 {
        eprintln!("skipped {n} {reason}");
    }
}

/// # Safety
/// Safe to call from, and intended to be, before or after `main`.
pub(super) fn skip_event(event: Event, reason: SkipReason) {
    use SkipReason::*;
    match reason {
        BeforeMain => {
            EVENTS_SKIPPED_BEFORE_MAIN.fetch_add(1, Ordering::Relaxed);
        }
        AfterMain => {
            // This is after `main`, so it's safe to use things like `eprintln!`,
            // which uses `ReentrantMutex` internally, which may use `pthread` mutexes.
            WARNED_AFTER_MAIN.get_or_init(|| {
                eprintln!("skipping {reason}");
            });
            eprintln!("skipped event after `main`: {:?}", event.kind);
        }
    };
}
