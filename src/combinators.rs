//! # Compose parsers into new parsers
//!
//! Functions in this module take existing parsers as arguments and create new parsers.
//!
//! To use the same parser several times you can use muatble references:
//!
//! This fails:
//! ```compile_fail
//! # use omnomnomicon::prelude::*;
//! let hello = literal("hello ");
//! let p1 = pair(hello, literal("world"));
//! let p2 = pair(hello, literal("omnomnomicon"));
//! let both = or(p1, p2);
//!
//! # Ok::<(), String>(())
//! ```
//!
//! This works:
//! ```rust
//! # use omnomnomicon::prelude::*;
//! let mut hello = literal("hello ");
//! let p1 = pair(literal("hello "), literal("world"));
//! let p2 = pair(literal("hello "), literal("omnomnomicon"));
//! let both = or(p1, p2);
//! let r = parse_result(both, "hello omnomnomicon")?;
//! // ("hello ", "omnomnomicon")
//! # assert_eq!(r, ("hello ", "omnomnomicon"));
//! # Ok::<(), String>(())
//! ```
//!
//! But better approach is to reduce parallel branches whenever possible
//! ```rust
//! # use omnomnomicon::prelude::*;
//! let hello = literal("hello ");
//! let p1 = literal("world");
//! let p2 = literal("omnomnomicon");
//! let both = or(p1, p2);
//! let p = pair(hello, both);
//! let r = parse_result(p, "hello omnomnomicon")?;
//! // ("hello ", "omnomnomicon")
//! # assert_eq!(r, ("hello ", "omnomnomicon"));
//! # Ok::<(), String>(())
//! ```

use crate::*;

/// Parallel combination of two parsers
///
/// Tries to parse input with both given parsers, succeeds if either of those succeeds. Inner
/// parsers could be primitive such as [`literal`][crate::parsers::literal] and composite such as [`between`]
/// or [`tuple`][tuple()].
/// Inner parser will backtrack if both contain components that succeded, but once whole `or`
/// succeeds on the first branch - it won't ever try to parse on the second one so longer parser
/// should go first. See also [`choice`][choice()] for a version that combines multiple parsers.
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let mut parser = or(literal("help"), literal("hello"));
///
/// let r = parse_result(&mut parser, "hello")?;
/// // "hello"
/// # assert_eq!(r, "hello");
///
/// let r = parse_result(&mut parser, "help")?;
/// // "help"
/// # assert_eq!(r, "help");
///
/// let r = parse_result(&mut parser, "helios").is_err();
/// // true
/// # assert!(r);
/// # Ok::<(), String>(())
/// ```
pub fn or<F1, F2, R>(mut f1: F1, mut f2: F2) -> impl FnMut(&str) -> Result<R>
where
    F1: FnMut(&str) -> Result<R>,
    F2: FnMut(&str) -> Result<R>,
{
    move |input| {
        let err1 = match f1(input) {
            Err(err) => err,
            ok => return ok,
        };

        let err2 = match f2(input) {
            Err(err) => err,
            ok => return ok,
        };
        Err(err1 + err2)
    }
}

/// Returns result from first of two sequential parsers
///
/// Applies two parses sequentially, fails if either of them fails, returns results from the first
/// one. See also [`snd`] and [`between`].
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let input = "hello world";
/// let parser = fst(literal("hello"), literal(" world"));
/// let r = parse_result(parser, input)?;
/// // "hello"
/// # assert_eq!(r, "hello");
/// # Ok::<(), String>(())
/// ```
pub fn fst<F1, F2, R1, R2>(f1: F1, f2: F2) -> impl FnMut(&str) -> Result<R1>
where
    F1: FnMut(&str) -> Result<R1>,
    F2: FnMut(&str) -> Result<R2>,
{
    fmap(|x| x.0, pair(f1, f2))
}

/// Returns result from second of two sequential parsers
///
/// Applies two parses sequentially, fails if either of them fails, returns results from the second
/// one. See also [`fst`] and [`between`].
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let input = "hello world";
/// let parser = snd(literal("hello "), literal("world"));
/// let r = parse_result(parser, input)?;
/// // "world"
/// # assert_eq!(r, "world");
/// # Ok::<(), String>(())
/// ```
pub fn snd<F1, F2, R1, R2>(f1: F1, f2: F2) -> impl FnMut(&str) -> Result<R2>
where
    F1: FnMut(&str) -> Result<R1>,
    F2: FnMut(&str) -> Result<R2>,
{
    fmap(|x| x.1, pair(f1, f2))
}

/// Returns result from middle of three sequential parsers
///
/// Applies three parses sequentially, fails if either of them fails, returns results from the second
/// one. Note, middle parser is specified last. See also [`fst`] and [`snd`].
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let input = "\"hello\"";
/// let parser = between(dquote, dquote, literal("hello"));
/// let r = parse_result(parser, input)?;
/// // "hello"
/// # assert_eq!(r, "hello");
/// # Ok::<(), String>(())
/// ```

pub fn between<F1, F2, F3, R1, R2, R3>(
    mut first: F1,
    mut last: F3,
    mut parser: F2,
) -> impl FnMut(&str) -> Result<R2>
where
    F1: FnMut(&str) -> Result<R1>,
    F2: FnMut(&str) -> Result<R2>,
    F3: FnMut(&str) -> Result<R3>,
{
    move |input| {
        let (output, _) = first(input)?;
        let (output, r) = output.bind(&mut parser)?;
        let (output, _) = output.bind(&mut last)?;
        //    fmap(|x: (R1, R2, R3)| x.1, tuple((first, parser, last)))
        Ok((output, r))
    }
}

/// Returns results from second of two space separated parses
///
/// Applies two parsers sequentially consuming a space between the inputs.
/// See also [`words`] and [`snd`]
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p1 = literal("price");
/// let p2 = number::<u32>;
/// let mut p = snd_word(p1, p2);
/// let r = parse_result(&mut p, "price 123")?;
/// // 123
/// # assert_eq!(r, 123);
///
/// let r = parse_result(&mut p, "price123");
/// // fails to parse - no space present
/// # assert_eq!(r.is_err(), true);
/// # Ok::<(), String>(())
/// ```
pub fn snd_word<P1, P2, R1, R2>(mut first: P1, mut second: P2) -> impl FnMut(&str) -> Result<R2>
where
    P1: FnMut(&str) -> Result<R1>,
    P2: FnMut(&str) -> Result<R2>,
{
    move |input| {
        let (output, _) = first(input)?;
        let consumed = input.len() > output.input.len();
        let (output, r2) = output.bind_space(consumed, &mut second)?;
        Ok((output, r2))
    }
}

/// Parser preceeded by a literal
///
/// First try to parse a static literal `tag` from the input then a space, then the `parser`.
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = tagged("weight", number::<u32>);
/// let r = parse_result(p, "weight 123")?;
/// // 123
/// # assert_eq!(r, 123);
/// # Ok::<(), String>(())
/// ```
pub fn tagged<P, R>(tag: &'static str, mut parser: P) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
{
    move |input| {
        let (output, _) = crate::parsers::literal(tag)(input)?;
        output.bind_space(!tag.is_empty(), &mut parser)
    }
}

/// Run a parser without consuming any input
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let check = words((number::<u16>, number::<u16>));
/// // take next 5 as long as they form two space separated numbers
/// let mut p = snd(peek(check), take(5));
/// let r = parse_result(&mut p, "12 56")?;
/// // "12 56"
/// # assert_eq!(r, "12 56");
///
/// let r = parse_result(&mut p, "12356").is_err();
/// // fails to parse
/// # assert!(r);
///
/// # Ok::<(), String>(())
/// ```
pub fn peek<F, R>(mut parser: F) -> impl FnMut(&str) -> Result<R>
where
    F: FnMut(&str) -> Result<R>,
{
    move |input| {
        let res = parser(input)?;
        Ok((Output::disabled(input), res.1))
    }
}

/// Run two parsers sequentially
///
/// If both parsers succeed - results will be returned as a tuple, It's a specialized
/// version of [`tuple`][tuple()] that works with two parsers, see also [`words`] and [`perm`].
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let mut p = pair(number::<u16>, literal("px"));
///
/// let r = parse_result(&mut p, "123px")?;
/// // (123, "px")
/// # assert_eq!(r, (123, "px"));
///
/// let r = parse_result(&mut p, "123p").is_err();
/// // fails to parse due to second element
/// # assert!(r);
/// # Ok::<(), String>(())
/// ```
pub fn pair<F1, F2, R1, R2>(mut first: F1, mut second: F2) -> impl FnMut(&str) -> Result<(R1, R2)>
where
    F1: FnMut(&str) -> Result<R1>,
    F2: FnMut(&str) -> Result<R2>,
{
    move |input| {
        let (output, r1) = first(input)?;
        let (output, r2) = output.bind(&mut second)?;
        Ok((output, (r1, r2)))
    }
}

/// Turn failing parser for `R` into succeding that returns `<Option<R>>`
///
/// If parser `P` fails outer parser will return `None` without consuming any input, if it succeeds
/// - `option` will consume used input and return `Some(R)`.
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let mut p = option(number::<u32>);
/// let r = parse_result(&mut p, "1234")?;
/// // Some(1234)
/// # assert_eq!(r, Some(1234));
///
/// let r = parse_result(&mut p, "potato")?;
/// // None
/// # assert_eq!(r, None);
/// # Ok::<(), String>(())
/// ```
pub fn option<P, R>(mut parser: P) -> impl FnMut(&'_ str) -> Result<'_, Option<R>>
where
    P: FnMut(&'_ str) -> Result<'_, R>,
{
    move |input| match parser(input) {
        Ok((output, r)) => Ok((output, Some(r))),
        Err(Terminate::Failure(_)) => Ok((Output::disabled(input), None)),
        Err(Terminate::Eof(state)) => Ok((Output::new(input, state), None)),
    }
}

#[test]
fn words_preserve_partial_match_info() {
    use crate::prelude::*;
    let w = option(literal("potato"));
    let mut p = words((literal("a"), w, number::<u32>));
    let h = parse_hints(&mut p, "a p").unwrap().replacements();
    assert_eq!(&h, &["potato"]);
}

#[test]
fn pwords_preserve_partial_match_info() {
    use crate::prelude::*;
    let w = option(literal("potato"));
    let p = pwords((literal("a"), w, number::<u32>));

    let h = parse_hints(p, "a p").unwrap().replacements();
    assert_eq!(&h, &["potato"]);
}

/// Apply parser multiple times, collect results into a Vec
///
/// Longer description that explains what function does, reference
/// to other functions and types
/// # Examples
/// ```rust
/// use omnomnomicon::prelude::*;
/// let p = many(literal("Na"));
/// let r = parse_result(p, "NaNaNaNa")?;
/// // ["Na", "Na", "Na", "Na"]
/// # assert_eq!(r, ["Na", "Na", "Na", "Na"]);
/// # Ok::<(), String>(())
/// ```
/// # Panics
/// With `sanity` feature enabled this parser will panic if subparser
/// returns a result without consuming any input.
pub fn many<F, R>(mut parser: F) -> impl FnMut(&str) -> Result<Vec<R>>
where
    F: FnMut(&str) -> Result<R>,
{
    move |mut input| {
        let mut res = Vec::new();
        let mut state = State::disabled();
        loop {
            if let Ok((output, r)) = parser(input) {
                if output.input.len() == input.len() {
                    #[cfg(feature = "sanity")]
                    {
                        panic!("subparser used in 'many' must reduce remaining input");
                    }
                    // to keep rust analyzer quiet
                    #[allow(unreachable_code)]
                    {
                        return Terminate::fail(
                            input,
                            "Subparser MUST consume something in order to succeed",
                        );
                    }
                }
                res.push(r);
                input = output.input;
                state = output.state;
            } else {
                return Ok((Output::new(input, state), res));
            }
        }
    }
}

/// Transforms results of parser `P` (`Result<A>`) with function `Fn(A) -> B` to get `Result<B>`
///
/// Failed parsing results are left unchanged. See also [`constmap`] and [`parse_with`].
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = words((number::<u32>, number::<u32>));
/// let f = |(a, b)| a + b + 1;
///
/// let r = parse_result(fmap(f, p), "12 34")?;
/// // 47
/// # assert_eq!(r, 47);
/// # Ok::<(), String>(())
/// ```
pub fn fmap<F, P, A, B>(mut transform: F, mut parser: P) -> impl FnMut(&str) -> Result<B>
where
    F: FnMut(A) -> B,
    P: FnMut(&str) -> Result<A>,
{
    move |input| {
        let (output, r) = parser(input)?;
        Ok((output, transform(r)))
    }
}

/// Process results of a parser with a failing function
///
/// Applies postprocessing to a parsing results with a function that can use `?` operator
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// # use std::str::FromStr;
/// # use std::num::ParseIntError;
/// fn external(s: String) -> core::result::Result<u32, ParseIntError> {
///     let r = u32::from_str(&s)?;
///     Ok(r * 2)
/// };
/// let p = parse_with(external, take_rest);
/// let r = parse_result(p, "1234")?;
/// // 2468
/// # assert_eq!(r, 2468);
/// # Ok::<(), String>(())
/// ```
///
/// ```rust
/// # use omnomnomicon::prelude::*;
/// fn external(s: u32) -> core::result::Result<u32, &'static str> {
///     if s > 1000 {
///         Err("Too big")
///     } else if s < 500 {
///         Err("Too small")
///     } else {
///         Ok(s)
///     }
/// };
/// let p = parse_with(external, number::<u32>);
/// let r = parse_result(&p, "700")?;
/// // 700
/// # assert_eq!(r, 700);
///
/// let e = parse_result(&p, "10000").unwrap_err();
/// // "Too big"
/// # assert_eq!(e, "Too big");
///
/// # Ok::<(), String>(())
/// ```
pub fn parse_with<F, P, A, B, E>(external: F, parser: P) -> impl Fn(&str) -> Result<B>
where
    P: Fn(&str) -> Result<A>,
    F: Fn(A) -> core::result::Result<B, E>,
    E: std::fmt::Display,
{
    move |input| {
        let (i, a) = parser(input)?;
        match external(a) {
            Ok(b) => Ok((i, b)),
            Err(err) => Terminate::fail(input, err.to_string()),
        }
    }
}

/// Replaces successful parse results with a constant
///
/// Convenient to replace parsed string literals with strongly typed values, see also [`fmap`] and
/// [`parse_with`].
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let t = constmap(true, literal("true"));
/// let f = constmap(false, literal("false"));
/// let mut b = label("bool", or(t, f));
///
/// let r = parse_result(&mut b, "true")?;
/// // true
/// assert_eq!(r, true);
///
/// let r = parse_result(&mut b, "false")?;
/// // false
/// assert_eq!(r, false);
/// # Ok::<(), String>(())
/// ```
pub fn constmap<P, A, B>(replacement: B, mut parser: P) -> impl FnMut(&'_ str) -> Result<'_, B>
where
    P: FnMut(&'_ str) -> Result<'_, A>,
    B: Clone,
{
    move |input| Ok((parser(input)?.0, replacement.clone()))
}

mod choice;
mod permutate;
mod pwords;
mod sequence;

pub use choice::*;
pub use permutate::*;
pub use pwords::*;
pub use sequence::*;
