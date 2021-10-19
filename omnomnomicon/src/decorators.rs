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
pub fn map_info<F, R, M>(mut parser: F, mut map: M) -> impl FnMut(&str) -> Result<R>
where
    F: FnMut(&str) -> Result<R>,
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
pub fn map_info_with<F, R, M>(parser: F, map: M) -> impl Fn(&str) -> Result<R>
where
    F: Fn(&str) -> Result<R>,
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
pub fn label<F, R>(label: &'static str, parser: F) -> impl FnMut(&str) -> Result<R>
where
    F: FnMut(&str) -> Result<R>,
{
    map_info(parser, move |state| state.push_label(label))
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

/// Adds a dynamic hint label for parser F
///
pub fn with_label<F, L, R>(mut label_fn: L, parser: F) -> impl FnMut(&str) -> Result<R>
where
    L: FnMut() -> Option<Cow<'static, str>>,
    F: FnMut(&str) -> Result<R>,
{
    map_info(parser, move |state| {
        if let Some(msg) = label_fn() {
            state.push_label(msg)
        }
    })
}

/// remove labels from the parser
pub fn unlabel<F, R>(parser: F) -> impl FnMut(&str) -> Result<R>
where
    F: FnMut(&str) -> Result<R>,
{
    map_info(parser, move |state| state.wipe_labels())
}

/// Insert completion info from a function
pub fn complete<F, R, P, I>(completer: P, parser: F) -> impl Fn(&str) -> Result<R>
where
    F: Fn(&str) -> Result<R>,
    P: Fn(&str) -> I,
    I: IntoIterator<Item = Comp>,
{
    map_info_with(parser, move |input, info| {
        let end = info.items.len();
        info.items
            .insert_many(end, completer(input).into_iter().map(Info::Comp))
    })
}

/// Add a help message to a parser
pub fn help<F, R>(message: &'static str, mut parser: F) -> impl FnMut(&str) -> Result<R>
where
    F: FnMut(&str) -> Result<R>,
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
        Err(err) => Err(err),
        Ok((mut output, r)) if output.input == "?" => {
            output.state.push(Info::Help(message));
            Ok((output, r))
        }
        ok => ok,
    }
}

/// Add a help message to a parser
pub fn optional_help<F, R>(
    message: Option<&'static str>,
    mut parser: F,
) -> impl FnMut(&str) -> Result<R>
where
    F: FnMut(&str) -> Result<R>,
{
    move |input| {
        let err = match parser(input) {
            Err(err) => err,
            ok => return ok,
        };
        let message = match message {
            Some(msg) => msg,
            None => return Err(err),
        };
        match &err {
            Terminate::Failure(failure) => {
                if failure.consumed_from(input) {
                    let help = Info::Help(message);
                    Err(Terminate::from(help))
                } else {
                    Err(err)
                }
            }
            Terminate::Eof(_) => Err(err),
        }
    }
}

/// Observe successfuly parsed value
///
/// A specialized version of [`fmap`][crate::combinators::fmap], but instaead of saving results - result is ignored
pub fn observe<F, P, A>(transform: F, parser: P) -> impl Fn(&str) -> Result<A>
where
    F: Fn(&A),
    P: Fn(&str) -> Result<A>,
{
    move |input| {
        let (i, r) = parser(input)?;
        transform(&r);
        Ok((i, r))
    }
}

/// Change parser to accept only digits of arbitrary length
pub fn mask_digits<R, F>(parser: F) -> impl FnMut(&str) -> Result<R>
where
    F: FnMut(&str) -> Result<R>,
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
pub fn mask_digits_exact<R, F>(parser: F) -> impl FnMut(&str) -> Result<R>
where
    F: FnMut(&str) -> Result<R>,
{
    map_info(parser, |info| {
        info.items.push(('0'..='9').into());
        info.items.push(Info::DisplayMask("d".into()));
    })
}
