//! optee_time is a Rust library that provides access to the OP-TEE time API. It is used when
//! the building target is specified as "optee".

use optee_utee::time::Time;

const NANOS_PER_SEC: u32 = 1_000_000_000;

/// The above type is a struct called Duration that contains two fields, secs and nanos, representing a
/// duration of time in seconds and nanoseconds.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct Duration {
    /// seconds
    pub secs: u64,
    /// nanoseconds
    pub nanos: u32, // Always 0 <= nanos < NANOS_PER_SEC
}
impl Duration {
    /// Creates a new `Duration` from the specified number of seconds and additional nanoseconds.
    pub const fn new(secs: u64, nanos: u32) -> Duration {
        Duration { secs, nanos }
    }
    /// Creates a new `Duration` from the specified number of seconds.
    pub const fn from_secs(secs: u64) -> Duration {
        Duration { secs, nanos: 0 }
    }
    /// Computes the subtraction of two durations.
    pub const fn checked_sub(self, rhs: Duration) -> Option<Duration> {
        if let Some(mut secs) = self.secs.checked_sub(rhs.secs) {
            let nanos = if self.nanos >= rhs.nanos {
                self.nanos - rhs.nanos
            } else if let Some(sub_secs) = secs.checked_sub(1) {
                secs = sub_secs;
                self.nanos + NANOS_PER_SEC - rhs.nanos
            } else {
                return None;
            };
            if (nanos < NANOS_PER_SEC) {
                return Some(Duration { secs, nanos });
            }
        }
        None
    }
    /// Returns the number of whole seconds in the duration.
    pub const fn as_secs(&self) -> u64 {
        self.secs
    }
}

/// Defines the Instant type
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub(crate) struct Instant(Duration);

/// Defines the SystemTime type
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct SystemTime(Duration);

/// Defines the SystemTimeError type
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct SystemTimeError(Duration);

pub(crate) const UNIX_EPOCH: SystemTime = SystemTime(Duration::from_secs(0));

impl Instant {
    /// Returns an instant corresponding to "now".
    pub(crate) fn now() -> Instant {
        let mut time = Time::new();
        time.ree_time();
        Instant(
            Duration::new(time.seconds as u64, time.millis)
        )
    }

    /// Returns an instant corresponding to zero.
    pub(crate) const fn zero() -> Instant {
        Instant(Duration::from_secs(0))
    }
}

impl SystemTime {
    /// Returns the system time corresponding to "now".
    pub fn now() -> SystemTime {
        let mut time = Time::new();
        time.ree_time();
        SystemTime(
            Duration::new(time.seconds as u64, time.millis)
        )
    }
    /// Returns the amount of time elapsed from another system time to this one.
    pub fn duration_since(&self, earlier: SystemTime) -> Result<Duration, SystemTimeError> {
        match self.0.checked_sub(earlier.0) {
            Some(dur) => Ok(dur),
            None => Err(SystemTimeError(Duration::from_secs(0))),
        }
    }
    /// Returns the seconds field of this `SystemTime`.
    pub fn secs(&self) -> u64 {
        self.0.secs
    }
}

impl std::fmt::Display for SystemTimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "optee_time::SystemTimeError: {}", self.0.secs)
    }
}
