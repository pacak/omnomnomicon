//! # Primitive parsers for taking data of the input stream

use std::borrow::Cow;

use crate::combinators::constmap;
use crate::*;

/// Parse up to `cnt` first characters of the input
///
/// Returned output will be cnt characters or shorter if input is shorter.
/// [`label`][crate::decorators::label]
/// annotations will be captured if input is shorter than expected. This parser won't fail.
/// See also [`take_exact`] if failing on shoter inputs is required.
/// # Examples
///
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = label("triple", take(3));
/// let r = parse_result(&p, "12")?;
/// // "12"
/// # assert_eq!(r, "12");
/// let r = parse_hints(&p, "12")?.labels();
/// // with "triple" label
/// # assert_eq!(r, ["triple"]);
///
/// let r = parse_result(&p, "123")?;
/// // "123"
/// # assert_eq!(r, "123");
/// let err = parse_hints(&p, "123");
/// # assert!(err.is_err());
///
/// # Ok::<(), String>(())
/// ```
pub fn take(cnt: usize) -> impl Fn(&str) -> Result<String> {
    move |input| {
        let mut last = 0;
        let mut cur_cnt = 0;
        for chr in input.chars().take(cnt) {
            last += chr.len_utf8();
            cur_cnt += 1;
        }
        let output = Output::maybe_enabled(&input[last..], cur_cnt < cnt);
        Ok((output, input[..last].to_string()))
    }
}

/// Parse exact `cnt` first characters of the input
///
/// Parser will fail with annotations if input is shorter than expected. See also [`take`].
/// # Examples
///
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = label("triple", take_exact(3));
/// let r = parse_hints(&p, "12")?.labels();
/// // fails with label "triple"
/// # assert_eq!(&r, &["triple"]);
///
/// let r = parse_result(&p, "123")?;
/// // "123"
/// # assert_eq!(r, "123");
/// let r = parse_hints(&p, "123");
/// // no labels
/// # assert!(r.is_err());
///
/// # Ok::<(), String>(())
/// ```
pub fn take_exact(cnt: usize) -> impl Fn(&str) -> Result<String> {
    move |input| {
        let res = take(cnt)(input)?;
        if res.0.state.is_enabled() {
            Terminate::eof()
        } else {
            Ok(res)
        }
    }
}

/// Consume remaining input as a String
///
/// Won't fail, will capture annotations. See also [`take`] and [`take_exact`].
/// # Examples
///
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let r = parse_result(take_rest, "123")?;
/// // 123
/// # assert_eq!(r, "123");
/// # Ok::<(), String>(())
/// ```
pub fn take_rest(input: &str) -> Result<String> {
    Ok((Output::enabled(""), input.to_string()))
}

/// Parse a possibly empty prefix of the string that satisfies the predicate
///
/// Won't fail, will capture annotations if reaches end of input.
/// See also [`take_while1`]
/// # Examples
///
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let r = parse_result(take_while(|c| c.is_ascii_digit()), "123abc")?;
/// // 123
/// # assert_eq!(r, "123");
/// # Ok::<(), String>(())
/// ```
pub fn take_while<P>(predicate: P) -> impl Fn(&str) -> Result<String>
where
    P: Fn(char) -> bool,
{
    move |input| {
        let output = input.trim_start_matches(&predicate);
        let prefix_len = input.len() - output.len();
        let res = &input[..prefix_len];
        let output = Output::maybe_enabled(output, output.is_empty());
        Ok((output, res.to_string()))
    }
}

/// Parse a non empty prefix of the string that satisfies the predicate
///
/// Will fail potentially capturing annotations if predicate is not satisfied.
/// See also [`take_while1`]
/// # Examples
///
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = label("digits", take_while1(|c| c.is_ascii_digit()));
/// let r = parse_result(&p, "123abc")?;
/// // 123
/// # assert_eq!(r, "123");
/// let err = parse_hints(&p, "123abc");
/// // no hints, labels
/// # assert!(err.is_err());
///
/// let p = label("digits", take_while1(|c| c.is_ascii_digit()));
/// let r = parse_result(&p, "12")?;
/// // "12"
/// # assert_eq!(r, "12");
/// let r = parse_hints(&p, "12")?.labels();
/// # assert_eq!(r, ["digits"]);
///
/// let err = parse_hints(&p, "x");
/// // fails with error message
/// # assert!(err.is_err());
///
/// let r = parse_hints(&p, "")?.labels();
/// // fails with "digits" label
/// # assert_eq!(r, ["digits"]);
///
/// # Ok::<(), String>(())
/// ```
pub fn take_while1<P>(predicate: P) -> impl Fn(&str) -> Result<String>
where
    P: Fn(char) -> bool,
{
    move |input| {
        let (output, res) = take_while(&predicate)(input)?;
        if res.is_empty() {
            if input.is_empty() {
                Terminate::eof()
            } else {
                Terminate::fail(input, "predicate failed")
            }
        } else {
            Ok((output, res))
        }
    }
}

/// Runs nested parser, fails if either nested parser fails or predicate on the output fails
pub fn guard<P, F, R>(predicate: P, parser: F) -> impl Fn(&str) -> Result<R>
where
    P: Fn(&R) -> bool,
    F: Fn(&str) -> Result<R>,
{
    move |input| {
        let (output, r) = parser(input)?;
        if predicate(&r) {
            Ok((output, r))
        } else {
            Terminate::fail(input, "guard failed")
        }
    }
}

/// returns true if located at the end of the string, useful to annotating places
/// with complex nested parsers
pub fn is_eof(input: &str) -> Result<bool> {
    let output = Output::maybe_enabled(input, input.is_empty());
    Ok((output, input.is_empty()))
}

/// Signify end of line
///
/// Place this as a last element of a tuple or decorate a parser `p` with
/// `fst(p, eof)` to ensure parser consumes  all the remaining input or fails.
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = fst(number::<u32>, eof);
///
/// let r = parse_result(&p, "123")?;
/// // 123
/// # assert_eq!(r, 123);
///
/// let r = parse_result(&p, "123x");
/// // fails
/// # assert_eq!(r.is_err(), true);
/// # Ok::<(), String>(())
/// ```
/// # Panics
/// iff there's panic, unwrap, etc
/// # Errors
/// # Safety
/// # Aborts
/// # Undefined Behavior
pub fn eof(input: &str) -> Result<()> {
    if input.is_empty() {
        Ok((Output::disabled(input), ()))
    } else {
        Terminate::fail(input, "Expected EOF")
    }
}

/// Parses a fixed string of the input or generate completion information
///
/// See also [`hliteral`]
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = literal("help");
/// let r = parse_result(&p, "help")?;
/// // "help"
/// # assert_eq!(r, "help");
/// let r = parse_result(&p, "not help").is_err();
/// // fails to parse
/// # assert_eq!(r, true);
/// # Ok::<(), String>(())
/// ```
pub fn literal(pattern: &'static str) -> impl Fn(&'_ str) -> Result<'_, &'static str> {
    use crate::utils::*;
    move |input| match check_prefix(pattern, input) {
        PrefixCheck::Match => Ok((Output::disabled(&input[pattern.len()..]), pattern)),
        PrefixCheck::Partial => Err(Terminate::from(Comp::simple(pattern, input.len()))),
        PrefixCheck::Mismatch => Terminate::fail(input, "literal not found"),
    }
}

/// Parse one or more spaces as `()`
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = space;
/// let r = parse_result(&p, " ")?;
/// // ()
/// # assert_eq!(r, ());
/// let r = parse_result(&p, "").is_err();
/// // fails to parse
/// # assert_eq!(r, true);
/// # Ok::<(), String>(())
/// ```
pub fn space(input: &str) -> Result<()> {
    let trimmed = input.trim_start();
    if trimmed.len() == input.len() {
        Err(Terminate::from(Comp::simple(" ", input.len())))
    } else {
        Ok((Output::disabled(trimmed), ()))
    }
}

/// Parses a fixed string of the input without generating completion information
///
/// Can be useful to hide parts or complete levels of commands, see also [`literal`]
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = hliteral("help");
/// let r = parse_result(&p, "help")?;
/// // "help"
/// # assert_eq!(r, "help");
/// let r = parse_result(&p, "not help").is_err();
/// // fails to parse
/// # assert_eq!(r, true);
/// # Ok::<(), String>(())
/// ```
pub fn hliteral(pattern: &'static str) -> impl Fn(&str) -> Result<&'static str> {
    move |input| {
        if let Some(rest) = input.strip_prefix(pattern) {
            Ok((Output::disabled(rest), pattern))
        } else {
            Terminate::fail(input, "literal not found")
        }
    }
}

/// Parse a string literal as a typed value
///
/// Parses a given static string literal of the stream and if succeeds - returns a given value
/// instead. See also [`constmap`] and [`literal`].
///
/// `tag(val, string) === constmap(val, literal(string))`.
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = or(tag(true, "yes"), tag(false, "no"));
/// let r = parse_result(p, "no")?;
/// // false
/// # assert_eq!(r, false);
/// # Ok::<(), String>(())
/// ```
pub fn tag<V>(value: V, pattern: &'static str) -> impl Fn(&'_ str) -> Result<'_, V>
where
    V: Clone,
{
    constmap(value, literal(pattern))
}

/// Parse a string literal as a typed value without generating completion information
///
/// Parses a given static string literal of the stream and if succeeds - returns a given value
/// instead. See also [`constmap`] and [`hliteral`] .
///
/// `htag(val, string) === constmap(val, hliteral(string))`.
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = or(htag(true, "yes"), tag(false, "no"));
/// let r = parse_result(p, "no")?;
/// // false
/// # assert_eq!(r, false);
/// # Ok::<(), String>(())
/// ```
pub fn htag<V>(value: V, pattern: &'static str) -> impl Fn(&str) -> Result<V>
where
    V: Clone,
{
    constmap(value, hliteral(pattern))
}

/// Hide completion info from nested parsers
///
/// In some cases it might be useful to have a hidden variant of a command. This command
/// will remove completion and labeling functionality without breaking the parser.
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = or(literal("banana"), literal("potato"));
/// let ph = hide(&p);
///
/// let r = p("ban");
/// // fails, suggests to type "banana"
///
/// let r = ph("ban");
/// // fails without suggesting to type anything
/// # Ok::<(), String>(())
/// ```
pub fn hide<P, R>(parser: P) -> impl Fn(&str) -> Result<R>
where
    P: Fn(&str) -> Result<R>,
{
    crate::decorators::map_info(parser, |state| *state = State::disabled())
}

/// Parse a single `"` character
///
/// Returns a unit
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let r = parse_result(dquote, "\"")?;
/// // ()
/// # Ok::<(), String>(())
/// ```
pub fn dquote(input: &str) -> Result<()> {
    constmap((), char('"'))(input)
}

/// Parse a single `'` character
///
/// Returns a unit
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let r = parse_result(squote, "\'")?;
/// // ()
/// # Ok::<(), String>(())
/// ```
pub fn squote(input: &str) -> Result<()> {
    constmap((), char('\''))(input)
}

/// Parses any character of the stream
///
/// Consumes a single utf8 character. This parser can be further restricted with [`guard`].
/// There's also a version restricted to a single character: [`char`][char()].
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = guard(|c| c.is_ascii_digit(), anychar);
/// let r = parse_result(&p, "1")?;
/// // 1
/// assert_eq!(r, '1');
///
/// let r = parse_result(&p, "x").is_err();
/// // fails to parse
/// # assert_eq!(r, true);
/// # Ok::<(), String>(())
/// ```
///
/// Input is not limited to ASCII
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = tuple((anychar, anychar, anychar));
/// let r = parse_result(p, "口水鸡")?;
/// // ('口', '水', '鸡')
/// # assert_eq!(r, ('口', '水', '鸡'));
/// # Ok::<(), String>(())
/// ```
pub fn anychar(input: &str) -> Result<char> {
    if let Some(c) = input.chars().next() {
        Ok((Output::disabled(&input[c.len_utf8()..]), c))
    } else {
        Terminate::eof()
    }
}

/// Lookup key in a table given by an iterator
///
/// Given a table of mappings between strings and keys `lookup_key` tries to parse
/// a first matching string as a separate word from the input and return a corresponding key `K`.
/// It is possible to restruct amount of results produced by `lookup_key` using `limit`  argument.
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// # use std::borrow::Cow;
/// #[derive(Copy, Clone, Debug, Eq, PartialEq)]
/// enum Items {
///     Apple,
///     Banana,
///     Flammenwerfer,
///     Pineapple,
///     SafeWord,
/// }
///
/// use Items::*;
/// let table = &[
///     (Apple, Cow::from("apple")),
///     (Banana, Cow::from("banana")),
///     (Flammenwerfer, Cow::from("Flammenwerfer")),
///     (Pineapple, Cow::from("pineapple")),
///     (SafeWord, Cow::from("FLÜGGÅӘNKб€ČHIŒßØLĮÊN")),
/// ];
/// let p = lookup_key(table.iter().cloned(), 10);
///
/// let r = parse_result(&p, "apple")?;
/// // Apple
/// assert_eq!(r, Items::Apple);
///
/// let r = parse_result(&p, "FLÜGGÅӘNKб€ČHIŒßØLĮÊN")?;
/// // SafeWord
/// assert_eq!(r, Items::SafeWord);
///
/// let r = parse_result(&p, "F");
/// // fails but suggests "Flammenwerfer" and "FLÜGGÅӘNKб€ČHIŒßØLĮÊN"
///
/// # let comps = |s| {
/// #    let mut v = parse_hints(&p, s)
/// #        .unwrap()
/// #        .comps
/// #        .into_iter()
/// #        .map(|c| c.replacement)
/// #        .collect::<Vec<_>>();
/// #    v.sort();
/// #    v
/// # };
/// # assert_eq!(comps("F"), &["FLÜGGÅӘNKб€ČHIŒßØLĮÊN", "Flammenwerfer"]);
/// # Ok::<(), String>(())
/// ```
pub fn lookup_key<T, K>(table: T, limit: usize) -> impl Fn(&str) -> Result<K>
where
    T: Iterator<Item = (K, Cow<'static, str>)> + Clone,
    K: Clone,
{
    use crate::utils::*;
    move |input| {
        let mut state = State::disabled();
        let mut r = None;
        let mut comp_present = 0;

        for (key, pat) in table.clone() {
            match check_prefix(&pat, input) {
                PrefixCheck::Match => {
                    if at_word_boundary(&pat, input) {
                        r = Some((key.clone(), pat));
                    }
                }
                PrefixCheck::Partial => {
                    state.push(Comp::simple(pat, input.len()));
                    comp_present += 1;
                    if comp_present > limit {
                        break;
                    }
                }
                PrefixCheck::Mismatch => {}
            }
        }

        match r {
            Some((key, pat)) => {
                let input = &input[pat.len()..];
                Ok((Output { input, state }, key))
            }
            None => {
                if comp_present > 0 {
                    Err(Terminate::from(state))
                } else {
                    Terminate::fail(input, "lookup_key: Key not found")
                }
            }
        }
    }
}

/// Try to parse an item from a table and return it's index
///
/// A variant of [`lookup_key`] specialized to use element's index as it's key.
/// It is possible to restruct amount of results produced by `lookup` using `limit`  argument.
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// # use std::borrow::Cow;
///
/// let table = &[
///     ( Cow::from("apple")),
///     ( Cow::from("banana")),
///     ( Cow::from("Flammenwerfer")),
///     ( Cow::from("pineapple")),
///     ( Cow::from("FLÜGGÅӘNKб€ČHIŒßØLĮÊN")),
/// ];
/// let p = lookup(table.iter(), 10);
///
/// let r = parse_result(&p, "apple")?;
/// // 0
/// assert_eq!(r, 0);
///
/// let r = parse_result(&p, "FLÜGGÅӘNKб€ČHIŒßØLĮÊN")?;
/// // 4
/// assert_eq!(r, 4);
///
/// let r = parse_result(&p, "F");
/// // fails but suggests "Flammenwerfer" and "FLÜGGÅӘNKб€ČHIŒßØLĮÊN"
///
/// # let comps = |s| {
/// #    let mut v = parse_hints(&p, s)
/// #        .unwrap()
/// #        .comps
/// #        .into_iter()
/// #        .map(|c| c.replacement)
/// #        .collect::<Vec<_>>();
/// #    v.sort();
/// #    v
/// # };
/// # assert_eq!(comps("F"), &["FLÜGGÅӘNKб€ČHIŒßØLĮÊN", "Flammenwerfer"]);
/// # Ok::<(), String>(())
/// ```
pub fn lookup<'a, T>(table: T, limit: usize) -> impl Fn(&str) -> Result<usize>
where
    T: Iterator<Item = &'a Cow<'static, str>> + Clone,
{
    lookup_key(table.cloned().enumerate(), limit)
}

/// Parses a given character of the stream
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = char('x');
/// let r = parse_result(p, "x")?;
/// // 'x'
/// # assert_eq!(r, 'x');
/// # Ok::<(), String>(())
/// ```
pub fn char(value: char) -> impl Fn(&str) -> Result<char> {
    guard(move |c| *c == value, anychar)
}

/// Parse a number of multiple different types
///
///
/// # Examples
/// ```
/// # use omnomnomicon::prelude::*;
/// let r = parse_result(number::<u32>, "1234").unwrap();
/// assert_eq!(r, 1234);
///
/// let r = parse_result(number::<isize>, "-1234").unwrap();
/// assert_eq!(r, -1234);
/// ```
pub fn number<T>(input: &str) -> Result<T>
where
    T: lexical_core::FromLexical,
{
    match lexical_core::parse_partial::<T>(input.as_bytes()) {
        // ¯\_(ツ)_/¯
        Ok((_, 0)) => Terminate::fail(input, "not a valid number"),
        Ok((t, offset)) => {
            let input = &input[offset..];
            let state = if input.is_empty() {
                State::enabled()
            } else {
                State::disabled()
            };
            Ok((Output { input, state }, t))
        }
        Err(_) if input.is_empty() => Terminate::eof(),
        Err(_) => Terminate::fail(input, "not a valid number"),
    }
}
