//! Expose information about parsing state to user
//!
//! This library exposes several types of parsing state informatin:
//!
//! ## Label - a description for parser
//!
//! Label describes a type of input or parser expects without specifying a concrete value. For
//! example decimal number can be interpreted as a price or as a quantity:
//!
//! ```rust
//! # use omnomnomicon::prelude::*;
//! let price = label("price", number::<u32>);
//! let qty = label("qty", number::<u32>);
//! ```
//!
//! Label can describe several possible values as a type. Here parser `boolean` will use
//! label "bool" to indicate that either `"true"` or `"false"` strings are accepted.
//!
//! ```rust
//! # use omnomnomicon::prelude::*;
//! let t = tag(true, "true");
//! let f = tag(false, "false");
//! let boolean = label("bool", or(t, f));
//! ```
//!
//! Label can be attached to a simple parser or to a group of parsers, labels from
//! parallel branches will be collected for as long as those branches are possible with current
//! input state.
//!
//! ```rust
//! # use omnomnomicon::prelude::*;
//! let banana = label("fruit", literal("banana"));
//! let salmon = label("fish", literal("salmon"));
//! let mut p = or(banana, salmon);
//!
//! // no input, so both options are possible
//! let r = parse_hints(&mut p, "")?.labels();
//! // ["fish", "fruit"]
//! # assert_eq!(r, &["fish", "fruit"]);
//!
//! // only "banana" can match
//! let r = parse_hints(&mut p, "b")?.labels();
//! // ["fruit"]
//! # assert_eq!(r, &["fruit"]);
//!
//! // neither can match, no hints
//! let r = parse_hints(&mut p, "x");
//! // no hints
//! # assert_eq!(r.is_err(), true);
//!
//! # Ok::<(), String>(())
//! ```
//!
//! ## Completion - a set of potential inputs
//!
//! Completion allows the application to propose several potential inputs, possibly with a label
//! each and permits user to quicky fill it in.
//!
//! ```rust
//! # use omnomnomicon::prelude::*;
//! let banana = label("fruit", literal("banana"));
//! let salmon = label("fish", literal("salmon"));
//! let mut p = or(banana, salmon);
//!
//! // no input, so both options are possible
//! let r = parse_hints(&mut p, "")?.replacements();
//! // ["banana", "salmon"]
//! # assert_eq!(r, &["banana", "salmon"]);
//!
//! // only "banana" can match
//! let r = parse_hints(&mut p, "ba")?.replacements();
//! // ["banana"]
//! # assert_eq!(r, &["banana"]);
//!
//! // neither can match, no hints
//! let r = parse_hints(&mut p, "x");
//! // no hints
//! # assert_eq!(r.is_err(), true);
//! # Ok::<(), String>(())
//! ```
//! Completion info is automatically generated from parsers such as [`tag`][crate::parsers::tag]
//! and [`literal`][crate::parsers::literal] or provided by the application using
//! [`complete`][complete()] or methods described in [`dict_word`][crate::tutorial::dict_word].
//!
//! ## Unique replacement
//!
//! When there's only one replacement is possible library will try to present it to user in a way
//! that it can be filled in with a single key press
//!
//! ## Input mask indication - filter for incoming key presses
//!
//! Library allows to filter incoming key presses based on current parsing state, for example
//! if the only possible input at a current state is a decimal number - only numeric keys and
//! possibly spaces to end the number will be allowed.
//!
//! ## Help - additional information about parsing state presented to user on demand
//!
//! Parsers can be annotated with help messages describing a purpose of some sequence of commands,
//!
use std::borrow::Cow;

use crate::*;

/// Apply arbitrary transformation to [`State`] in [`Result`] if present
pub fn map_info<P, R, M>(mut parser: P, mut map: M) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
    M: FnMut(&mut State),
{
    move |input| {
        let mut res = parser(input);
        match &mut res {
            Ok((output, _)) if output.state.is_enabled() => map(&mut output.state),
            Ok(_) => {}
            Err(Terminate::Eof(state)) => map(state),
            Err(Terminate::Failure(_)) => {}
        }
        res
    }
}

/// Apply arbitrary transformation to [`State`] in [`Result`] if present
///
/// In addition to a reference to [`State`] transformation function takes current input
pub fn map_info_with<P, R, M>(mut parser: P, map: M) -> impl FnMut(&str) -> Result<R>
where
    P: for<'s> FnMut(&'s str) -> Result<'s, R>,
    M: Fn(&str, &mut State),
{
    move |input| {
        let mut res = parser(input);
        match &mut res {
            Ok((output, _)) if output.state.is_enabled() => map(input, &mut output.state),
            Ok(_) => {}
            Err(Terminate::Eof(state)) => map(input, state),
            Err(Terminate::Failure(_)) => {}
        }
        res
    }
}

/// Add a static label to a parser
///
/// If label needs to change depending on some state external to parser - see [`with_label`].
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = label("price", number::<u32>);
///
/// let r = parse_hints(p, "")?.labels();
/// // ["price"]
/// # assert_eq!(r, &["price"]);
/// # Ok::<(), String>(())
/// ```
pub fn label<P, R>(label: &'static str, parser: P) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
{
    map_info(parser, move |state| state.push_label(label))
}

/// Add a static label to a parser unless label already present
pub fn label_if_missing<P, R>(label: &'static str, parser: P) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
{
    map_info(parser, move |state| {
        if !state.status.has_label() {
            state.push_label(label)
        }
    })
}

/// Modify labels on nested parsers with a function
///
/// ```rust
/// # use omnomnomicon::prelude::*;
/// # use std::borrow::Cow;
/// let p = label("banana", number::<u32>);
/// let p2 = relabel(|l|*l = Cow::from(format!("{}{}", "super", l)), p);
///
/// let r = parse_hints(p2, "")?.labels();
/// // ["superbanana"]
/// # assert_eq!(r, &["superbanana"]);
/// # Ok::<(), String>(())
/// ```
pub fn relabel<L, P, R>(mut relabel: L, parser: P) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
    L: FnMut(&mut Cow<'static, str>),
{
    map_info(parser, move |state| {
        if state.status.has_label() {
            for i in state.items.iter_mut() {
                if let Info::Label(l) = i {
                    relabel(l)
                }
            }
        }
    })
}

/// Attach a static prefix to all the labels in the nested parsers
///
/// ```rust
/// # use omnomnomicon::prelude::*;
/// # use std::borrow::Cow;
/// let p = label("banana", number::<u32>);
/// let p2 = prefix_labels("super", p);
///
/// let r = parse_hints(p2, "")?.labels();
/// // ["superbanana"]
/// # assert_eq!(r, &["superbanana"]);
/// # Ok::<(), String>(())
/// ```
pub fn prefix_labels<P, R>(label: &'static str, parser: P) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
{
    relabel(move |l| *l = Cow::from(format!("{}{}", label, l)), parser)
}

/// Attach a static suffix to all the labels in the nested parsers
///
/// ```rust
/// # use omnomnomicon::prelude::*;
/// # use std::borrow::Cow;
/// let p = label("super", number::<u32>);
/// let p2 = suffix_labels("banana", p);
///
/// let r = parse_hints(p2, "")?.labels();
/// // ["superbanana"]
/// # assert_eq!(r, &["superbanana"]);
/// # Ok::<(), String>(())
/// ```
pub fn suffix_labels<P, R>(label: &'static str, parser: P) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
{
    relabel(move |l| *l = Cow::from(format!("{}{}", l, label)), parser)
}

/// Adds a dynamic label for parser F
///
/// Labels are better suited to describe what the parsers are.
/// Only the most outer label is kept for any parser combinations,
/// label will be used to report expected items
///
/// See also [`with_hint`]
pub fn with_label<P, L, R>(mut label_fn: L, parser: P) -> impl FnMut(&str) -> Result<R>
where
    L: FnMut() -> Option<Cow<'static, str>>,
    P: FnMut(&str) -> Result<R>,
{
    map_info(parser, move |state| {
        if let Some(msg) = label_fn() {
            state.push_label(msg)
        }
    })
}

/// Adds a dynamic hint for parser F
///
/// Hints are better suited to describe possible values such as current
/// state of item, etc.
///
/// See also [`with_label`]
pub fn hint<P, L, R>(hint: &'static str, parser: P) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
{
    map_info(parser, move |state| {
        state.items.push(Info::Hint(Cow::Borrowed(hint)))
    })
}

/// Adds a staic hint for parser F
///
/// Hints are better suited to describe possible values such as current
/// state of item, etc.
///
/// See also [`label`]
pub fn with_hint<P, L, R>(mut hint: L, parser: P) -> impl FnMut(&str) -> Result<R>
where
    L: FnMut() -> Option<Cow<'static, str>>,
    P: FnMut(&str) -> Result<R>,
{
    map_info(parser, move |state| {
        if let Some(msg) = hint() {
            state.items.push(Info::Hint(msg))
        }
    })
}

/// remove labels from the parser
pub fn unlabel<P, R>(parser: P) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
{
    map_info(parser, move |state| state.wipe_labels())
}

/// Insert completion info from a function
pub fn complete<P, R, C, I>(completer: C, parser: P) -> impl for<'s> FnMut(&'s str) -> Result<'s, R>
where
    P: for<'s> FnMut(&'s str) -> Result<'s, R>,
    C: Fn(&str) -> I,
    I: IntoIterator<Item = Comp>,
{
    map_info_with(parser, move |input, info| {
        let end = info.items.len();
        info.items
            .insert_many(end, completer(input).into_iter().map(Info::Comp))
    })
}

/// Add a help message to a parser
pub fn help<P, R>(message: &'static str, mut parser: P) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
{
    move |input| match parser(input) {
        Err(Terminate::Failure(failure)) => {
            Err(if input.ends_with('?') && failure.consumed_from(input) {
                let help = Info::Help(message);
                Terminate::from(help)
            } else {
                Terminate::Failure(failure)
            })
        }
        Err(f @ Terminate::Eof(_)) => {
            if !input.is_empty() {
                let help = Terminate::from(Info::Help(message));
                Err(f + help)
            } else {
                Err(f)
            }
        }
        Ok((mut output, r)) if output.input == "?" => {
            output.state.push(Info::Help(message));
            Ok((output, r))
        }
        ok => ok,
    }
}

/// Observe successfuly parsed value
///
/// A specialized version of [`fmap`][crate::combinators::fmap], but instaead of saving results - result is ignored
pub fn observe<F, P, A>(transform: F, mut parser: P) -> impl FnMut(&str) -> Result<A>
where
    F: Fn(&A),
    P: for<'s> FnMut(&'s str) -> Result<'s, A>,
{
    move |input| {
        let (i, r) = parser(input)?;
        transform(&r);
        Ok((i, r))
    }
}

/// Change parser to accept only digits of arbitrary length
pub fn mask_digits<R, P>(parser: P) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
{
    map_info(parser, |info| {
        info.items.push(('0'..='9').into());
        info.items.push(Info::KeyMask(' '.into()));
        info.items.push(Info::DisplayMask("d".into()));
    })
}

/// Change parser to accept only digits of fixed length
///
/// Since entering spaces is not allowed - `parser` must terminate on it's own after
/// consuming a certain amount of digits.
pub fn mask_digits_exact<R, P>(parser: P) -> impl FnMut(&str) -> Result<R>
where
    P: FnMut(&str) -> Result<R>,
{
    map_info(parser, |info| {
        info.items.push(('0'..='9').into());
        info.items.push(Info::DisplayMask("d".into()));
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prelude::*;
    #[test]
    fn help_on_literal() {
        let p1 = help("help for place", words((literal("place"), u32::parse)));
        let p2 = help("help for perm", words((literal("perm"), u32::parse)));
        let mut p = choice((p1, p2));

        // two potential help messages collide
        let f = parse_hints(&mut p, "p").unwrap();
        assert!(f.help.is_none());

        // single possible help message produces result
        let f = parse_hints(&mut p, "place?").unwrap();
        assert!(f.help.is_some());

        // single possible help message produces result, this _should_ be none though
        // because as soon as user types ? p parser fails
        let f = parse_hints(&mut p, "pl").unwrap();
        assert!(f.help.is_some());
    }

    #[test]
    fn test_highlight_for_incorrect_portion() {
        let mut p = words((literal("hello"), literal("world"), literal("x")));
        assert_eq!(parse_hints(&mut p, "hello ww").unwrap_err().offset, 2);
        assert_eq!(parse_hints(&mut p, "hello worldd").unwrap_err().offset, 1);
        assert_eq!(parse_hints(&mut p, "hello world he").unwrap_err().offset, 2);
    }
}
