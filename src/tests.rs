//! Utility functions for writing tests and doctests
//!
//! Functions contained in this module are not meant to be used directly but are used to write
//! tests and demonstration examples

use crate::complete::{apply_parser, Hints, ParseOutcome};
use crate::*;

/// Try produce a result given a parser and a string
///
/// Applies parser to a string and tries to summarize the error if parser fails. Since each
/// individial parser is a function `Fn(&str) -> Result<R>` use of [`parse_result`] is not
/// required but a convenience.
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let r = parse_result(number::<u32>, "123")?;
/// // 123
/// # assert_eq!(r, 123);
///
/// let r = parse_result(literal("hello"), "123").is_err();
/// // fails to parse
/// # assert_eq!(r, true);
/// # Ok::<(), String>(())
/// ```
/// # Errors
/// If parser does not produces a result this function this will produce a string that might
/// contain parser's expectations
pub fn parse_result<P, R>(mut parser: P, input: &str) -> core::result::Result<R, String>
where
    P: FnMut(&str) -> Result<R>,
{
    match parser(input) {
        Ok((_, r)) => Ok(r),
        Err(Terminate::Eof(_)) => Err("Not enough input".to_string()),
        Err(Terminate::Failure(f)) => Err(f.message.into()),
    }
}

/// Try to produce parse hints given a parser and a string
///
/// Only useful to extract information about parsing: [`Hints`]. See also [`apply_parser`].
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p1 = label("label 1", literal("hello"));
/// let p2 = label("label 2", literal("help"));
/// let p3 = label("label 3", literal("world"));
/// let p = choice((p1, p2, p3));
/// let r = parse_hints(p, "he")?.labels();
/// // ["label 1", "label 2"]
/// # assert_eq!(&r, &["label 1", "label 2"]);
/// # Ok::<(), String>(())
/// ```
pub fn parse_hints<P, R>(parser: P, input: &str) -> core::result::Result<Hints, Failure>
where
    P: FnMut(&str) -> Result<R>,
{
    match apply_parser(parser, input) {
        Some(ParseOutcome::Hints(hints)) => Ok(hints),
        Some(ParseOutcome::Failure(x)) => Err(x),
        None => Err(Failure::from("No hints generated")),
    }
}

/// Try to produce a `Failure` given a parser and a string
///
/// Only useful to extract information about parsing: [`Failure`]. See also [`apply_parser`].
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p1 = label("label 1", literal("hello"));
/// let p2 = label("label 2", literal("help"));
/// let p3 = label("label 3", literal("world"));
/// let mut p = choice((p1, p2, p3));
/// let r = parse_failure(&mut p, "potato")?.message;
/// // "literal not found"
/// # assert_eq!(r, "literal not found");
/// # Ok::<(), String>(())
/// ```
pub fn parse_failure<P, R>(parser: P, input: &str) -> core::result::Result<Failure, String>
where
    P: FnMut(&str) -> Result<R>,
{
    match apply_parser(parser, input) {
        Some(ParseOutcome::Failure(x)) => Ok(x),
        _ => Err(String::from("No failure generated")),
    }
}
