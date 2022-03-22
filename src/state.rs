//! Information collected by the parser
//!
//!

// go home clippy, you're drunk
#![allow(clippy::suspicious_op_assign_impl)]
#![allow(clippy::suspicious_arithmetic_impl)]

use crate::Result;
use smallvec::SmallVec;
use std::borrow::Cow;
use std::ops::{Add, AddAssign, RangeInclusive};

/// Holds leftover input plus parsing state
///
/// To write your own primitive parsers or parser combinators see [`Output::bind`] and
/// [`Output::bind_space`]
/// See also [`State`]
#[derive(Debug)]
pub struct Output<'a> {
    /// remaining input
    pub input: &'a str,
    /// current parsing state
    pub state: State,
}

impl<'a> Output<'a> {
    /// Check if whole input was consumed
    ///
    /// When parsing a string you want to consume the whole input by the time
    /// you done parsing
    pub fn is_empty(&self) -> bool {
        self.input.is_empty()
    }

    /// Create `Output` with this `input` and [`State`] in collecting mode
    #[inline]
    pub fn enabled(input: &'a str) -> Self {
        Self {
            input,
            state: State::enabled(),
        }
    }

    /// Create `Output` with this `input` and [`State`] in passive mode
    #[inline]
    pub fn disabled(input: &'a str) -> Self {
        Self {
            input,
            state: State::disabled(),
        }
    }

    /// Create `Output` with this `input`, set [`State`] mode to collecting or passive
    #[inline]
    pub fn maybe_enabled(input: &'a str, enable: bool) -> Self {
        Self {
            input,
            state: if enable {
                State::enabled()
            } else {
                State::disabled()
            },
        }
    }

    /// Create `Output` with this `input` and `State`
    #[inline]
    pub fn new(input: &'a str, state: State) -> Self {
        Self { input, state }
    }

    /// Parser chaining primitive
    ///
    /// To run two parsers sequentially application must pass remaining output from the first
    /// parser to the second parser and combine completion info if necessary.
    /// combination.
    ///
    /// A variant of [`pair`][crate::combinators::pair] combinator can be implemented as following:
    /// # Examples
    /// ```rust
    /// # use omnomnomicon::prelude::*;
    /// // individual subparsers
    /// let mut p1 = literal("hello");
    /// let p2 = literal(" world");
    ///
    /// let input = "hello world";
    ///
    /// let (output, r1) = p1(input)?;
    /// let (output, r2) = output.bind(p2)?;
    /// let result = (r1, r2);
    /// // ("hello", " world");
    /// # assert_eq!(r1, "hello");
    /// # assert_eq!(r2, " world");
    /// // At this moment parser can return `Ok((output, result))`
    /// # Ok::<(), Terminate>(())
    /// ```
    #[inline]
    pub fn bind<P, R>(self, mut parser: P) -> Result<'a, R>
    where
        P: FnMut(&str) -> Result<R>,
    {
        match parser(self.input) {
            Ok((output, r)) => Ok((output + self.state, r)),
            Err(err) => Err(err + self.state),
        }
    }

    /// Whitespace aware parser chaining primitive that deals with spaces
    ///
    /// between two consuming parsers there must be at lest one space
    /// Outer edges are considered as non consuming
    /// Space is consumed by the right consuming entry

    pub fn bind_space<P, R>(self, consumed: bool, mut parser: P) -> Result<'a, R>
    where
        P: FnMut(&str) -> Result<R>,
    {
        //
        fn try_trimmed<P, R>(mut parser: P, input: &str) -> Result<R>
        where
            P: FnMut(&str) -> Result<R>,
        {
            let trimmed = input.trim_start();
            if input.len() > trimmed.len() {
                let (mut output, r) = parser(trimmed)?;
                if output.input.len() == trimmed.len() {
                    output.input = input;
                }
                Ok((output, r))
            } else if input.is_empty() {
                Err(Terminate::from(Comp::simple(" ", input.len())))
            } else {
                Err(Terminate::default())
            }
        }

        if consumed {
            // in case when one of the previous parsers consumed there's two possible options
            // - next parser consumes trimmed input
            // - next parser consumes empty input
            //
            // This needs to be done in two separate calls since inputs are different
            let err = match try_trimmed(&mut parser, self.input) {
                Ok((o, r)) => {
                    return Ok((o + self.state, r));
                }
                Err(err) => err,
            };

            match parser("") {
                // _o contains completion at truncated input which but unless
                // they start this space - they can't be entered here.
                Ok((_o, r)) => Ok((self, r)),
                // if parser can't accept empty - error here doesn't matter
                // since only continuation of the previous parser or space
                // are accepted.
                Err(_) => match err {
                    Terminate::Eof(state) => Err(Terminate::Eof(state + self.state)),
                    Terminate::Failure(err) => {
                        if self.state.is_enabled() {
                            Err(Terminate::Eof(self.state))
                        } else {
                            Err(Terminate::Failure(err))
                        }
                    }
                },
            }
        } else {
            // otherwise
            // - next parser consumes raw input
            // - next parser consumes empty input
            //
            // Both of those cases are handled by the same call to parser
            match parser(self.input) {
                Ok((output, r)) => Ok((output + self.state, r)),

                // failing without info means we accept only previous parser
                // or throw an error for current one if previous is done
                Err(Terminate::Failure(err)) => {
                    if self.state.is_enabled() {
                        Err(Terminate::Eof(self.state))
                    } else {
                        Err(Terminate::Failure(err))
                    }
                }
                // failing with info means we can try to keep accepting previous info
                // or accepting info for the next parser
                Err(Terminate::Eof(next)) => Err(Terminate::Eof(next + self.state)),
            }
        }
    }
}

impl AddAssign<State> for Output<'_> {
    #[inline]
    fn add_assign(&mut self, rhs: State) {
        self.state += rhs
    }
}

impl Add<State> for Output<'_> {
    type Output = Self;

    #[inline]
    fn add(mut self, rhs: State) -> Self::Output {
        self += rhs;
        self
    }
}

/// Parsing state - collection of hints, completions, help messages, etc.
///
/// `State` starts collecting information when parser reaches end of available input and is still
/// ready to accept more. This state can be created with [`State::enabled()`]. When parser succeeds
/// in a way it can't possibly accept more input or fails in a way that no additional input can
/// make it pass `State` will ignore any decorations. This state can be crated with
/// [`State::disabled()`]
///
/// Forms a monoid with [`State::disabled()`] and [`Add`] trait.
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// // this parser accepts some (non zero) spaces followed by 'x' and more spaces
/// // (this can be written in regexp form as `/\s\+x\s*/`), parser returns number
/// // of spaces before 'x'
/// fn useful_parser(input: &str) -> Result<usize> {
///     let trimmed = input.trim_start();
///
///     let spaces = input.len() - trimmed.len();
///     // we fail if there's no spaces at the beginning of the string
///     if spaces == 0 && input.len() > 0 {
///        return Terminate::fail(input, "spaces expected")
///     }
///
///     // then we check if dot is present. 3 cases are possible:
///     // - trimmed input is empty - then we are waiting for a 'x'
///     // - trimmed input starts with 'x' - we are done
///     // - trimmed input starts with something else - we fail
///
///     if trimmed.is_empty() {
///         // there are some spaces but nothing useful left
///         Terminate::eof()
///     } else if let Some(rest) = trimmed.strip_prefix('x') {
///         // 'x' is here, we can eat up remaining spaces
///         let trimmed = rest.trim_start();
///         if trimmed.is_empty() {
///            // there are only spaces after x, we succeed but can accept more
///            Ok((Output::enabled(trimmed), spaces))
///         } else {
///            // there are non space characters, we can't possibly accept more
///            Ok((Output::disabled(trimmed), spaces))
///         }
///     } else {
///         Terminate::fail(input, "expected 'x'")
///     }
/// }
///
/// let mut p = label("useful", useful_parser);
///
/// let r = parse_result(&mut p, " x");
/// // Ok(1), "useful" label is active since more spaces will be accepted
/// # assert_eq!(r, Ok(1));
///
/// let r = parse_result(&mut p, "  x  ");
/// // Ok(2), "useful" label is active since more spaces will be accepted
/// # assert_eq!(r, Ok(2));
/// # assert_eq!(parse_hints(&mut p, " x ")?.labels(), &["useful"]);
///
/// let r = parse_result(&mut p, "");
/// // fails, "useful" label is active since preceding spaces will be accepted
/// # assert_eq!(r.is_err(), true);
/// # assert_eq!(parse_hints(&mut p, "")?.labels(), &["useful"]);
///
/// let r = parse_result(&mut p, "nope");
/// // fails, no labels since no input can be matched
/// # assert_eq!(r.is_err(), true);
///
/// let r = parse_result(&mut p, " x .");
/// // Ok(1), no labels since parser is fully satisfied
/// # assert_eq!(r, Ok(1));
/// # assert_eq!(parse_hints(&mut p, " x .").is_err(), true);
/// # Ok::<(), String>(())
/// ```
///
/// ```will_panic
/// # use omnomnomicon::prelude::*;
/// let r = sub_done_but("wrong", ());
/// // will panic in debug mode
/// ```
#[derive(Debug)]
pub struct State {
    /// Bitmask with information about status and `items`'s contents
    pub status: Status,
    /// Collection of accumulated parser information
    pub items: SmallVec<[Info; 1]>,
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.is_enabled() || self.items.is_empty() {
            write!(f, "No input expected at this point")
        } else if self.status.has_label() {
            write!(f, "Expected one of ")?;
            let mut sep = false;
            for item in self.items.iter() {
                if let Info::Label(label) = item {
                    if sep {
                        write!(f, ", ")?;
                    }
                    write!(f, "'{}'", label)?;
                    sep = true;
                }
            }
            Ok(())
        } else {
            write!(f, "Expected one of ")?;
            let mut sep = false;
            for item in self.items.iter() {
                if let Info::Comp(comp) = item {
                    if sep {
                        write!(f, ", ")?;
                    }
                    write!(f, "'{}'", comp.replacement)?;
                    sep = true;
                }
            }
            Ok(())
        }
    }
}

impl State {
    #[inline]
    /// Should [`State`] be collecting information
    pub fn is_enabled(&self) -> bool {
        self.status.is_enabled()
    }

    #[inline]
    /// Collect information, see [`State`]
    pub fn enabled() -> Self {
        Self {
            status: Status::ENABLED,
            items: SmallVec::default(),
        }
    }
    #[inline]
    /// Ignore information, see [`State`]
    pub fn disabled() -> Self {
        Self {
            status: Status::DISABLED,
            items: SmallVec::default(),
        }
    }

    #[inline]
    /// Add a new information item, most types are supported except
    /// for those requiring additional handling such as label: [`push_label`][State::push_label].
    pub fn push<T>(&mut self, val: T)
    where
        Info: From<T>,
    {
        self.items.push(Info::from(val));
        self.status.enable();
    }

    /// Add a new label, this removes existing labels
    pub fn push_label<T>(&mut self, val: T)
    where
        Cow<'static, str>: From<T>,
    {
        if self.status.has_label() {
            self.items.retain(|i| !i.is_label());
        }
        self.items.push(Info::Label(Cow::from(val)));
        self.status.insert(Status::HAS_LABEL);
    }

    /// Remove existing labels
    #[inline]
    pub fn wipe_labels(&mut self) {
        if self.status.has_label() {
            self.items.retain(|i| !i.is_label());
        }
        self.status.remove(Status::HAS_LABEL);
    }

    /// Set "help available" flag
    pub fn enable_help(&mut self) {
        self.status.insert(Status::HAS_HELP);
    }

    /// check if help is blocked
    ///
    /// Help will be blocked when two branches containing one are combined
    pub fn help_blocked(&self) -> bool {
        self.status.contains(Status::HELP_BLOCKED)
    }
}

pub use status::*;
mod status {
    use bitflags::bitflags;

    bitflags! {
        /// Bitmask containing information about state
        pub struct Status: u8 {
            /// Collect information, see [`State`]
            const IS_ENABLED = 0b_0000_0001;

            /// labels are present, mostly a performance thing
            const HAS_LABEL  = 0b_0000_0010;

            /// help is present, a performance thing
            const HAS_HELP = 0b_0000_0100;

            /// set when parser tries to combine
            /// two branches each containing help
            const HELP_BLOCKED = 0b_0000_1000;
        }
    }

    impl Status {
        /// Collect information, see [`State`]
        pub const ENABLED: Self = Self::IS_ENABLED;

        /// Ignore information, see [`State`]
        pub const DISABLED: Self = Self::empty();

        /// Check if collection is enabled, see [`State`]
        pub fn is_enabled(self) -> bool {
            self.contains(Self::IS_ENABLED)
        }

        /// Enable information collection, see [`State`]
        pub fn enable(&mut self) {
            self.insert(Self::IS_ENABLED);
        }

        /// Check if labels are present.
        pub fn has_label(self) -> bool {
            self.contains(Self::HAS_LABEL)
        }

        /// Check if help message is available.
        pub fn has_help(self) -> bool {
            self.contains(Self::HAS_HELP)
        }

        /// Check if help message is available.
        pub fn help_blocked(self) -> bool {
            self.contains(Self::HELP_BLOCKED)
        }
    }
}
/// Parser failed
///
/// Forms a monoid with [`Terminate::default()`] and [`Add`] trait
#[derive(Debug)]
pub enum Terminate {
    /// Parser has failed but it can succeed given more input. [`State`] contains information about
    /// labels, help messages, completion and so on.
    Eof(State),

    /// Parser has failed and can't succeed even if given more input.
    Failure(Failure),
}

impl Default for Terminate {
    fn default() -> Self {
        Terminate::Failure(Failure::default())
    }
}

impl std::fmt::Display for Terminate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminate::Eof(e) => e.fmt(f),
            Terminate::Failure(e) => e.fmt(f),
        }
    }
}

impl Terminate {
    /// Parser has failed and can't succeed even if given more input.
    ///
    /// `input` should contain all the input parser can't understand
    pub fn fail<R, M>(input: &str, msg: M) -> core::result::Result<R, Terminate>
    where
        M: Into<Cow<'static, str>>,
    {
        Err(Terminate::Failure(Failure {
            message: msg.into(),
            offset: input.len(),
        }))
    }

    /// Parser has failed but can succeed given more input. Parser will start collecting labels,
    /// help messages. See [`State`]
    pub fn eof<R>() -> core::result::Result<R, Terminate> {
        Err(Terminate::Eof(State::enabled()))
    }
}

impl AddAssign for State {
    #[inline]
    fn add_assign(&mut self, mut rhs: Self) {
        if rhs.is_enabled() {
            self.status += rhs.status;
            self.items.append(&mut rhs.items);
        }
    }
}

impl Add for State {
    type Output = Self;

    #[inline]
    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign for Status {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        if self.has_help() && rhs.has_help() {
            self.insert(Status::HELP_BLOCKED);
        }

        *self |= rhs;
    }
}

impl Add for Status {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        self | rhs
    }
}

impl AddAssign<Self> for Terminate {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        match (&mut *self, rhs) {
            (Terminate::Failure(f1), Terminate::Failure(f2)) => *f1 += f2,
            (Terminate::Failure(_f1), Terminate::Eof(i2)) => {
                if i2.is_enabled() {
                    *self = Terminate::Eof(i2)
                }
            }
            (Terminate::Eof(_), Terminate::Failure(_)) => {}
            (Terminate::Eof(i1), Terminate::Eof(i2)) => *i1 += i2,
        }
    }
}

impl Add for Terminate {
    type Output = Self;

    #[inline]
    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}
impl AddAssign<State> for Terminate {
    #[inline]
    fn add_assign(&mut self, rhs: State) {
        match &mut *self {
            Terminate::Eof(state) => *state += rhs,
            Terminate::Failure(_) => {
                if rhs.is_enabled() {
                    *self = Terminate::Eof(rhs)
                }
            }
        }
    }
}

impl Add<State> for Terminate {
    type Output = Self;

    #[inline]
    fn add(mut self, rhs: State) -> Self::Output {
        self += rhs;
        self
    }
}

impl From<State> for Terminate {
    #[inline]
    fn from(state: State) -> Self {
        Terminate::Eof(state)
    }
}

impl From<Comp> for Terminate {
    #[inline]
    fn from(comp: Comp) -> Self {
        Terminate::Eof(State::from(comp))
    }
}

impl From<Info> for Terminate {
    #[inline]
    fn from(info: Info) -> Self {
        Terminate::Eof(State::from(info))
    }
}

impl From<Comp> for State {
    #[inline]
    fn from(comp: Comp) -> Self {
        let mut state = State::enabled();
        state.items.push(Info::Comp(comp));
        state
    }
}

impl From<Comp> for Info {
    #[inline]
    fn from(comp: Comp) -> Self {
        Info::Comp(comp)
    }
}

impl From<Info> for State {
    #[inline]
    fn from(info: Info) -> Self {
        let mut state = State::enabled();
        if let Info::Help(_) = info {
            state.status.insert(Status::HAS_HELP);
        }
        state.items.push(info);
        state
    }
}

/// Failure information on current parser chain
///
/// Forms a Monoid picking better error messages with [`Add`] and [`Default`] traits
#[derive(Debug, Default, Clone)]
pub struct Failure {
    /// Error message text
    pub message: Cow<'static, str>,
    /// Length of the invalid part of the string (in bytes) from the end.
    ///
    /// In other words assuming the input string was `"Hello world"` and parser expects
    /// `"Hello World"` then invalid part is `"world"` and offset is going to be 5.
    pub offset: usize,
}

impl std::fmt::Display for Failure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Failure {
    /// Check if failure has happened immediately or after successfuly parsing something
    ///
    /// See for [`help`][crate::decorators::help] for usage, generally not for end user usage.
    pub fn consumed_from(&self, input: &str) -> bool {
        input.len() > self.offset
    }
}

impl From<Failure> for String {
    fn from(f: Failure) -> Self {
        f.message.into()
    }
}
impl Add<Failure> for Failure {
    type Output = Failure;

    fn add(mut self, rhs: Failure) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign for Failure {
    fn add_assign(&mut self, rhs: Self) {
        if self.message.is_empty() || self.offset >= rhs.offset {
            *self = rhs
        }
    }
}

/// A single bit of information about current subparser state
#[derive(Debug)]
pub enum Info {
    /// Completion information, see [complete][crate::decorators::complete]
    Comp(Comp),
    /// Help message, see [`help`][crate::decorators::help]
    Help(&'static str),
    /// Custom rendering for input filtering, see [`DisplayMask`]
    DisplayMask(DisplayMask),
    /// Input filtering primitive, see [`KeyMask`]
    KeyMask(KeyMask),
    /// Static or dynamic label, see [`label`][crate::decorators::label]
    Label(Cow<'static, str>),
}

impl Info {
    /// Returns `true` if the info is [`Comp`].
    pub fn is_comp(&self) -> bool {
        match self {
            Self::Comp(..) => true,
            _ => false,
        }
    }

    /// Returns `true` if the info is `Label`.
    pub fn is_label(&self) -> bool {
        match self {
            Self::Label(..) => true,
            _ => false,
        }
    }
}

/// Completion information with custom rendering
#[derive(Clone, Debug)]
pub struct Comp {
    /// Includes existing input as well as characters to be inserted
    pub replacement: Cow<'static, str>,
    /// Custom rendering for this completion
    ///
    /// For example when completing an id number for an item from a list of
    /// shopping items this can be used to display that "1" corresponds to "milk",
    /// "2" corrensponds to bread, etc.
    pub display: Cow<'static, str>,
    /// Length of the remaining input at completion point
    pub remaining: usize,
}

impl From<Comp> for String {
    fn from(comp: Comp) -> Self {
        String::from(comp.replacement)
    }
}

/// Used for key filtering visualization
#[derive(Clone, Debug)]
pub struct DisplayMask {
    /// A custom representation for active key filtering. If [`DisplayMask`] is not specified
    /// but filtering is present - parser creates a replacement based on the actual contents
    ///
    /// See [`mask_digits`][crate::decorators::mask_digits]
    pub display: Cow<'static, str>,
}

impl From<&'static str> for DisplayMask {
    fn from(msg: &'static str) -> Self {
        DisplayMask {
            display: msg.into(),
        }
    }
}

/// Used for filtering keypresses
///
/// See [`mask_digits`][crate::decorators::mask_digits]
#[derive(Clone, Debug, Copy)]
pub enum KeyMask {
    /// Matches a single character
    Char(char),
    /// Matches a character from inclusive range
    Range(char, char),
}

impl KeyMask {
    /// Check if a character satisfies a given keymask
    pub fn matches(&self, c: char) -> bool {
        match *self {
            KeyMask::Char(cc) => c == cc,
            KeyMask::Range(start, end) => start <= c && c <= end,
        }
    }
}

impl From<char> for KeyMask {
    fn from(c: char) -> Self {
        KeyMask::Char(c)
    }
}

impl From<RangeInclusive<char>> for KeyMask {
    fn from(r: RangeInclusive<char>) -> Self {
        KeyMask::Range(*r.start(), *r.end())
    }
}

impl From<RangeInclusive<char>> for Info {
    fn from(r: RangeInclusive<char>) -> Self {
        Info::KeyMask(r.into())
    }
}

impl Comp {
    /// Generate completion info with no custom display
    ///
    /// Most useful when you want to prompt user to enter a plain string
    /// with no additional meaning. Second argument points at the insertion place.
    /// Usually it's end of the input, but if expected input partially exists
    /// there's no need to modify the word.
    /// # Examples
    /// ```rust
    /// # use omnomnomicon::prelude::*;
    /// // Prompt for a space at the end of the input
    /// let input = "";
    /// let space = Comp::simple(" ", input.len());
    ///
    /// // Prompt to complete word banana
    /// let input = "a ban";
    /// let banana = Comp::simple("banana", 2);
    /// ```
    pub fn simple<M>(info: M, remaining: usize) -> Self
    where
        M: Into<Cow<'static, str>>,
    {
        let display = info.into();
        let replacement = display.clone();
        Comp {
            display,
            replacement,
            remaining,
        }
    }
}

#[test]
fn bind_space_preserves_potential_info() {
    use omnomnomicon::prelude::*;
    let mut p1 = option(literal("foo"));
    let p2 = option(literal("bar"));

    let (o, _r1) = p1("").unwrap();
    let (o, _r2) = o.bind_space(false, p2).unwrap();
    assert_eq!(o.state.items.len(), 2);
}
