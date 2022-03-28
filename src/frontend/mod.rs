//! # Frontends
//!
//! Library implements parsing and completion logic independently from user interaction logic.
//! Frontend is something that implements user interaction parts.
//!
//! Right now there's only [rustyline] frontend which uses <https://docs.rs/rustyline>

#[cfg(feature = "rustyline")]
pub mod rustyline;

#[cfg(feature = "tui")]
pub mod tui;
