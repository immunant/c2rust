use std::{
    fmt::{self, Display, Formatter},
    sync::atomic::{AtomicBool, AtomicU64, Ordering},
};

use crate::events::Event;

#[derive(Debug)]
pub enum SkipReason {
    BeforeMain,
    AfterMain,
}

impl Display for SkipReason {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let (before_or_after, array, tors) = match self {
            Self::BeforeMain => ("before", "init_array", "ctors"),
            Self::AfterMain => ("after", "fini_array", "dtors"),
        };
        write!(f, "events {before_or_after} `main`, likely due to static initializers run in the `.{array}` or `.{tors}` sections")
    }
}

static EVENTS_SKIPPED_BEFORE_MAIN: AtomicU64 = AtomicU64::new(0);
static WARNED_AFTER_MAIN: AtomicBool = AtomicBool::new(false);

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
            // # Async-signal-safety: atomic increments are safe.
            EVENTS_SKIPPED_BEFORE_MAIN.fetch_add(1, Ordering::Relaxed);
        }
        AfterMain => {
            // # Async-signal-safety: not really signal-safe, but if we
            // get a signal after `main` ends, we're probably fine.
            // The allocator should have enough free memory by now
            // to not need to call `mmap`.
            if !WARNED_AFTER_MAIN.swap(true, Ordering::Relaxed) {
                // WARNED_AFTER_MAIN was previously `false` but we swapped it,
                // which will happen exactly once per run so we can print now.
                eprintln!("skipping {reason}");
            }
            // TODO: It would be nice to get rid of the two `eprintln`s here
            // so we can guarantee signal safety, but then we would get no
            // debugging output.
            eprintln!("skipped event after `main`: {:?}", event.kind);
        }
    };
}
