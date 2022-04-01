//! Try to parse and extract information for user
//!
//!

use crate::*;
use std::borrow::Cow;

/// Collection of hints related to current parser state
///
/// Most of the fields can be configured via [decorators][crate::decorators].
#[derive(Clone, Debug)]
pub struct Hints {
    /// Labels of subparsers accepted in current state, see [`label`][crate::decorators::label].
    /// Field type is isomorphic to `[&str]`.
    pub labels: Vec<Cow<'static, str>>,

    /// Potential completions generated by subparsers
    pub comps: Vec<Comp>,

    /// Help message for inner most parser that began to consume input but not
    /// yet finally succeeded, see [`help`][crate::decorators::help]
    ///
    /// To use it you can use [`help()`][Hints::help()]
    pub help: Option<&'static str>,

    /// The fact that help message is available doesn't mean user expects it.
    /// By default user can query for help message by appending a single '?' symbol
    /// to the input, frontend implementation can use this flag to display
    /// the help message, but it can also ignore the flag and always display the message
    /// or not to display it at all
    pub help_requested: bool,

    /// The only possible completion available at current state
    pub replacement: Option<String>,

    /// Currently enabled key filters, if present only matching key pressed
    /// will be accepted
    pub key_mask: Vec<KeyMask>,

    /// Contains indication about active key masks
    pub display_mask: Option<Cow<'static, str>>,
}

impl Hints {
    /// Deduplicated and sorted vector of labels
    pub fn labels(self) -> Vec<String> {
        let mut res = self
            .labels
            .into_iter()
            .map(String::from)
            .collect::<Vec<_>>();
        res.sort();
        res.dedup();
        res
    }

    /// Deduplicated and sorted vector of expectations: labels + unique input, usually space if
    /// present
    pub fn expected(self) -> Vec<String> {
        let mut res = self
            .labels
            .into_iter()
            .map(String::from)
            .chain(self.replacement.into_iter())
            .collect::<Vec<_>>();
        res.sort();
        res.dedup();
        res
    }

    /// Deduplicated and sorted vector of completion replacements
    pub fn replacements(self) -> Vec<String> {
        let mut res = self.comps.into_iter().map(String::from).collect::<Vec<_>>();
        res.sort();
        res.dedup();
        res
    }

    /// current help if both available and requested
    pub fn help(&self) -> Option<&str> {
        if self.help_requested {
            self.help
        } else {
            None
        }
    }
}

/// Information about parsing outcome.
///
/// Does not contain the parse result itself and should be
/// used to generate hints to user as of what inputs parser can accept at current state
#[derive(Debug, Clone)]
pub enum ParseOutcome {
    /// Parser failed or succeeded but it's ready to accept more input. [`Hints`] contains
    /// information about expected data
    Hints(Hints),

    /// Parser failed, [`Failure`] contains error message and indicator to which part
    /// of input it applies. This condition usually means input needs to be modified
    Failure(Failure),

    /// Parser succeeded, you can run it directly on the input to produce a parsed value
    Success,
}

impl ParseOutcome {
    /// Return a reference to current completions, can be empty
    pub fn completions(&self) -> Option<&[Comp]> {
        match self {
            ParseOutcome::Hints(hints) => Some(&hints.comps),
            ParseOutcome::Failure(_) | ParseOutcome::Success => None,
        }
    }
    /// Return a reference to current completions, can't be empty
    pub fn non_empty_completions(&self) -> Option<&[Comp]> {
        let comps = self.completions()?;
        if comps.is_empty() {
            None
        } else {
            Some(comps)
        }
    }

    /// reference to hints, if outcome contains them
    pub fn hints(&self) -> Option<&Hints> {
        match self {
            ParseOutcome::Failure(_) | ParseOutcome::Success => None,
            ParseOutcome::Hints(g) => Some(g),
        }
    }

    /// reference to help if available and requested
    pub fn help(&self) -> Option<&str> {
        let hints = self.hints()?;
        if hints.help_requested {
            hints.help
        } else {
            None
        }
    }
}

/// Try to produce parse hints or failure info given parser and a string
///
/// Only useful to extract information about parsing. [`ParseOutcome`] represents
/// parsing failure and completion hints.
///
/// `None` means parser produced some output without any additional information.
///
/// See also [`apply_parser_rec`].
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p1 = label("label 1", literal("hello"));
/// let p2 = label("label 2", literal("help"));
/// let p3 = label("label 3", literal("world"));
/// let p = choice((p1, p2, p3));
/// let r = apply_parser(p, "he");
/// if let ParseOutcome::Hints(hints) = r {
///     // hints.labels
///     // ["label 1", "label 2"]
///     # assert_eq!(hints.labels(), ["label 1", "label 2"]);
/// }
/// # else {
/// # panic!("hints expected")
/// # }
/// # Ok::<(), String>(())
/// ```
pub fn apply_parser<P, R>(mut parser: P, input: &str) -> ParseOutcome
where
    P: FnMut(&str) -> Result<R>,
{
    let info = match parser(input) {
        Ok((output, _)) if !output.state.is_enabled() => return ParseOutcome::Success,
        Ok((output, _)) => output.state,
        Err(Terminate::Eof(state)) => state,
        Err(Terminate::Failure(err)) => return ParseOutcome::Failure(err),
    };

    let mut labels = Vec::new();
    let mut comps = Vec::new();
    let mut key_mask = Vec::new();
    let mut display_mask = None;
    let mut help = None;
    let help_blocked = info.help_blocked();
    for item in info.items {
        match item {
            Info::Comp(comp) => comps.push(comp),
            Info::Help(msg) => help = Some(msg),
            Info::DisplayMask(mask) => display_mask = Some(mask.display),
            Info::KeyMask(mask) => key_mask.push(mask),
            Info::Label(label) => labels.push(label),
        }
    }
    #[cfg(feature = "sanity")]
    {
        // sanity check - proposed completions should make sense
        for c in comps.iter() {
            let rep = &c.replacement;
            let rem = c.remaining;
            assert!(
                input.len() >= rem,
                "Can't have {} bytes remaining of {:?}",
                rem,
                input
            );
            let rem_input = &input[input.len() - rem..];
            assert!(
                rep.starts_with(rem_input),
                "{:?} can't be used to complete {:?}",
                rep,
                rem_input
            );
        }
    }
    let replacement = match comps.as_slice() {
        [unique] => Some(unique.replacement[unique.remaining..].to_owned()),
        _ => None,
    };
    let help_requested = input.ends_with('?');
    if help_blocked {
        help = None;
    }
    let hints = Hints {
        labels,
        help,
        comps,
        help_requested,
        replacement,
        display_mask,
        key_mask,
    };

    ParseOutcome::Hints(hints)
}

/// Try to produce parse hints or failure info given parser and a string
///
/// A variant of [`apply_parser`] that will try to apply single available replacement when present
/// and try to parse again, several times if possible. The only part of a result that will be used
/// is a single replacement allowing user to quick fill several independent simple completions
/// at expense of running the parser multiple times.
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p1 = literal("hello");
/// let p2 = literal("-");
/// let p3 = literal("world");
/// let p = words((p1, p2, p3));
/// let r = apply_parser_rec(p, "h");
/// if let ParseOutcome::Hints(hints) = r {
///     // hints.replacement
///     // "hello world"
///     # assert_eq!(hints.replacement.unwrap(), "ello - world");
/// }
/// # else {
/// # panic!("hints expected")
/// # }
/// # Ok::<(), String>(())
/// ```
pub fn apply_parser_rec<P, R>(mut parser: P, input: &str) -> ParseOutcome
where
    P: FnMut(&str) -> Result<R>,
{
    let mut hints = match apply_parser(&mut parser, input) {
        ParseOutcome::Hints(
            hints @ Hints {
                replacement: Some(_),
                ..
            },
        ) => hints,
        r => return r,
    };
    let mut new_input = format!("{}{}", input, &hints.replacement.as_ref().unwrap());
    for _ in 0..32 {
        match apply_parser(&mut parser, &new_input) {
            ParseOutcome::Hints(Hints {
                replacement: Some(rep),
                ..
            }) => new_input = format!("{}{}", &new_input, &rep),
            _ => {
                hints.replacement = Some(new_input[input.len()..].to_string());
                return ParseOutcome::Hints(hints);
            }
        }
    }
    hints.replacement = Some(new_input[input.len()..].to_string());
    ParseOutcome::Hints(hints)
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    #[test]
    fn bind_space_does_not_accept_spaces_in_bogus_places() {
        let mut p = words((literal("potat"), tag((), "x")));
        // at this point parser p can't possibly match
        assert!(parse_hints(&mut p, "potato").is_err());
    }

    #[test]
    fn generated_completions_are_sane() {
        fn p(input: &str) -> Result<()> {
            let tprice = snd(literal("p"), number::<u32>);
            let tqty = snd(literal("q"), number::<u32>);
            let cmd = pwords((tprice, tqty));
            fmap(|_| (), cmd)(input)
        }
        let f = parse_failure(&mut p, "\0\0\0").unwrap();
        assert_eq!(f.offset, 0);
    }

    #[test]
    fn completion_from_literals() {
        let p1 = literal("potato");
        let p2 = literal("banana");
        let p = or(p1, p2);
        let r = parse_hints(p, "ba").unwrap().replacements();
        assert_eq!(r, ["banana"]);
    }

    #[test]
    fn words_and_label_interactions() {
        let mut p1 = label("first", number::<u8>);
        assert!(parse_hints(&mut p1, "1 ").is_err());
        let mut p2 = label("second", number::<u8>);
        let mut p = words((&mut p1, &mut p2));

        assert_eq!(&parse_hints(&mut p, "").unwrap().labels(), &["first"]);
        assert_eq!(&parse_hints(&mut p, "1").unwrap().labels(), &["first"]);
        assert_eq!(&parse_hints(&mut p, "1 ").unwrap().labels(), &["second"]);
        assert_eq!(&parse_hints(&mut p, "1 2").unwrap().labels(), &["second"]);
        assert!(parse_hints(&mut p, "1 2 ").is_err());
    }

    #[test]
    fn words_and_comp_interactions() {
        let mut p1 = tag((), "t1");
        let mut p2 = tag((), "t2");
        let mut p3 = label("first", option(number::<u8>));
        let mut p4 = label("second", number::<u8>);
        let mut p = words((option(&mut p1), &mut p2, &mut p3, &mut p4));

        let comps = |h: Hints| {
            h.comps
                .into_iter()
                .map(|c| c.replacement.to_string())
                .collect::<Vec<_>>()
        };

        let none = <Vec<String>>::new();

        assert_eq!(&comps(parse_hints(&mut p, "").unwrap()), &["t2", "t1"]);
        assert_eq!(&comps(parse_hints(&mut p, "t").unwrap()), &["t2", "t1"]);
        assert_eq!(&comps(parse_hints(&mut p, "t1").unwrap()), &[" "]);
        assert_eq!(&comps(parse_hints(&mut p, "t1 ").unwrap()), &["t2"]);
        assert_eq!(&comps(parse_hints(&mut p, "t1 t2").unwrap()), &[" "]);
        assert_eq!(&comps(parse_hints(&mut p, "t1 t2 ").unwrap()), &none);
        assert_eq!(&comps(parse_hints(&mut p, "t1 t2 12").unwrap()), &[" "]);
        assert_eq!(&comps(parse_hints(&mut p, "t1 t2 12 ").unwrap()), &none);
        assert_eq!(&comps(parse_hints(&mut p, "t1 t2 12 1").unwrap()), &none);
    }
}
