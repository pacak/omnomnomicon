//! Backends
//!
//! Library implements parsing and completion logic independently from user interaction logic.
//! Backed is something that implements user interaction parts.
//!
//! Right now there's only [rustyline] backend which uses <https://docs.rs/rustyline>

#[cfg(feature = "rustyline")]
pub mod rustyline;
