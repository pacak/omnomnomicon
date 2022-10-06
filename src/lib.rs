#![allow(clippy::collapsible_else_if)]
#![allow(clippy::match_like_matches_macro)]
#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]

//! # Purpose
//! Omnomnomicon is a parser combinator library. Main goals are:
//! - working with interactive user input
//! - hints and labels based on current input
//! - automatic and manual autocompletion
//! - in-prompt-help and error messages
//!
//! # Points of interest
//! - [tutorial][mod@tutorial] gives an overview of most of the features
//! - [prelude][mod@prelude] reexports all the things
//! - [frontend][mod@frontend] contains things
//!
//! # Cargo Features
//!
//! ## Frontends
//! - [`frontend_tui`][crate::frontend::tui]
//!
//! ## Library support
//! - `enum-map` - [`Updater`][crate::Updater] for `EnumMap`
//! - `chrono` - [`Parser`] for various types
//!
//! ## Misc
//! - `tutorial` - all code from tutorial, used in tests
//! - `sanity` - a set of sanity checks, for internal sanity checks, mostly internal invariants.
//! Generally not needed but can help with debugging parsers with a strange behavior
//! - `debug` - provide a rendered parser state as part of completion hint

#[macro_use]
mod macros;

extern crate self as omnomnomicon;

#[cfg(feature = "chrono")]
#[doc(hidden)]
pub mod chrono;
pub mod combinators;
pub mod complete;
pub mod decorators;
pub mod frontend;
pub mod parsers;
pub mod prelude;
pub mod state;
#[cfg(feature = "tutorial")]
pub mod tutorial;
pub mod utils;
pub use crate::state::*;
pub mod editor;
pub mod tests;
pub use crate::{
    combinators::{choice, fmap, or, tagged, words},
    decorators::{help, hint, label, label_if_missing, with_hint},
    parsers::{literal, lookup_key, number, space, tag},
    patch::{apply_change, suffix_errors, updater_for, Checker, UpdateOrInsert, Updater},
};
pub use omnomnomicon_derive::{Parser, Updater};

pub mod patch;

/// A trait to parse an item in a generic way
///
/// Most of the primitive types can be parsed using this trait. Parsing is usually performed
/// without any external state, over full item domain and without any additional checks or prompts.
/// input.
///
/// Any item implementing [`Parser`] and [`Debug`][std::fmt::Debug] will be supported by
/// [`Updater`][Updater], the interactive data updater, see [patch][mod@patch].
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = label("floating point", f64::parse);
/// let r = parse_result(p, "3.1415")?;
/// // 3.1415
/// # assert_eq!(r, 3.1415);
/// # Ok::<(), String>(())
/// ```
pub trait Parser {
    /// Parse a value in a most generic form
    fn parse(input: &str) -> Result<Self>
    where
        Self: Sized;

    /// Perform a sanity check on a contained value
    fn sanity_check(&self, _errors: &mut Vec<String>) {}
}

/// Holds parsing results
///
/// Parsing results can be broken down into one of several categories and since there's no monads in rust
/// it's up to you to deal with it if you want to create your own primitive parsers.
///
/// 1. Parser can successfuly produce a result `R` This state is represented as `Ok((Output, R))` where
/// `Output` contains leftover input and might contain additional [`State`] information. You can
/// create [`Output`] using [`Output::enabled`], [`Output::disabled`] and [`Output::maybe_enabled`],
/// see [`State`] for more information
///
/// 2. Parser can fail due to insufficient input. `Result` stores this case as
/// `Err(Terminate::Eof(State))` and you can create it using [`Terminate::eof`]. See [`State`]. This
/// case means parser can succeed given more input and `State` usually contains information on what
/// input is expected.
///
/// 3. Parser can fail due to mismatching input. `Result` stores this case as
/// `Err(Terminate::Failure(Failure))`. You can create it with [`Terminate::fail`]. This case means
/// parser has encountered something unexpected and adding more input won't make it pass. [`Failure`]
/// contains information to exact part of the input it considers mismatching.
pub type Result<'a, R> = ::core::result::Result<(Output<'a>, R), Terminate>;

macro_rules! lexical {
    ($t:ty) => {
        /// Parses a number, parser fails if number overflows
        impl Parser for $t {
            fn parse(input: &str) -> Result<Self> {
                crate::parsers::number(input)
            }
        }
    };
}

lexical!(f32);
lexical!(f64);
lexical!(i8);
lexical!(i16);
lexical!(i32);
lexical!(i64);
lexical!(i128);
lexical!(isize);
lexical!(u8);
lexical!(u16);
lexical!(u32);
lexical!(u64);
lexical!(u128);
lexical!(usize);

macro_rules! nonzero {
    ($base:ty, $nz:ty) => {
        /// Parses a nonzero number, parser fails if number overflows or is zero
        impl Parser for $nz {
            fn parse(input: &str) -> Result<Self> {
                let (o, base) = crate::parsers::number::<$base>(input)?;
                match <$nz>::try_from(base) {
                    Ok(val) => Ok((o, val)),
                    Err(_) => Terminate::fail(input, "must be non zero"),
                }
            }
        }
    };
}

nonzero!(u8, std::num::NonZeroU8);
nonzero!(u16, std::num::NonZeroU16);
nonzero!(u32, std::num::NonZeroU32);
nonzero!(u64, std::num::NonZeroU64);
nonzero!(u128, std::num::NonZeroU128);
nonzero!(i8, std::num::NonZeroI8);
nonzero!(i16, std::num::NonZeroI16);
nonzero!(i32, std::num::NonZeroI32);
nonzero!(i64, std::num::NonZeroI64);
nonzero!(i128, std::num::NonZeroI128);

/// Parse a boolean
///
/// `"true"` for `true`, and `"false"` for `false`
impl Parser for bool {
    fn parse(input: &str) -> Result<Self> {
        or(tag(true, "true"), tag(false, "false"))(input)
    }
}

/// Parser for `()`
///
/// Consumes no input, always succeeds, collects no annotations
impl Parser for () {
    fn parse(input: &str) -> Result<Self> {
        Ok((Output::disabled(input), ()))
    }
}

/// Parser for [`Duration`][std::time::Duration]
///
/// Accepts a number followed by a suffix:
/// - `ns` - nanoseconds
/// - `us` - microseconds
/// - `ms` - milliseconds
/// - `s` - seconsd
///
/// Number can contain fractional part and must be positive.
impl Parser for std::time::Duration {
    fn parse(input: &str) -> Result<Self> {
        use self::prelude::*;
        #[derive(Clone)]
        enum Suffix {
            Nano,
            Micro,
            Milli,
            Secs,
        }
        let s1 = tag(Suffix::Nano, "ns");
        let s2 = tag(Suffix::Micro, "us");
        let s3 = tag(Suffix::Milli, "ms");
        let s4 = tag(Suffix::Secs, "s");
        let ss = choice((s1, s2, s3, s4));
        let (output, mut value) = <f64 as Parser>::parse(input)?;
        let (output, suffix) = output.bind(ss)?;
        match suffix {
            Suffix::Nano => value /= 1e9,
            Suffix::Micro => value /= 1e6,
            Suffix::Milli => value /= 1e3,
            Suffix::Secs => {}
        }
        // go home clippy, you're drunk
        #[allow(clippy::manual_range_contains)]
        if value < 0.0 || value > 1e18 {
            Terminate::fail(input, "not a valid duration")
        } else {
            Ok((output, std::time::Duration::from_secs_f64(value)))
        }
    }
}

impl Parser for String {
    /// Parse a quoted string
    ///
    /// Can contain quotes inside if escaped with '\', escape characters are included into result
    fn parse(input: &str) -> Result<Self> {
        use self::prelude::*;

        let mut escape = false;

        let body = take_while(|c| {
            if escape {
                escape = false;
                true
            } else if c == '\\' {
                escape = true;
                true
            } else {
                c != '"'
            }
        });

        let result = between(
            label("\"", char('"')),
            label("\"", char('"')),
            label("string", body),
        )(input)?;
        Ok(result)
    }
}
