#![allow(missing_docs)]

//! # Tutorial. Whys and hows
//!
//! This library uses parser combinators to crate interactive command line UI.
//!
//!
//! Parser combinator is a higher order function that accepts one or more parsers
//! as input and produces a parser as an output. Combinators can compose parsers
//! sequentially or give a choice between parsers in parallel.
//!
//! Given parsers `p` and `q` each consuming some specific input parser `pair(p, q)`
//! will first consume what's possible with `p` then leftovers will be passed to `q`.
//! If both parsers succeed their output will be produced as a tuple, if `p` fails
//! `q` won't be called, if `p` succeeds but `q` fails - output of `p` will be ignored.
//!
//! # Example
//! ```rust
//! # use omnomnomicon::prelude::*;
//! let p = literal("banana");
//! let q = number::<u32>;
//! let mut c = pair(p, q);
//!
//! let r = parse_result(&mut c, "banana13")?;
//! // ("banana", 13)
//! # assert_eq!(r, ("banana", 13));
//!
//! let r = parse_result(&mut c, "13banana");
//! // wrong order, parsing fails
//! # assert_eq!(r.is_err(), true);
//!
//! let r = parse_result(&mut c, "banana");
//! // p succeeds, q fails
//! # assert_eq!(r.is_err(), true);
//!
//! let r = parse_result(&mut c, "potato");
//! // p fails, q is never called
//! # assert_eq!(r.is_err(), true);
//!
//! # Ok::<(), String>(())
//! ```
//!
//! Here [`literal`] and [`number`] are primitive parsers and [`pair`] is a parser combinator. More
//! parsers can be found [here][crate::parsers] and combinators [here][crate::combinators].
//!
//! For interactive applications it is better to separate individual words in commands
//! by one or more spaces:
//!
//! # Example
//! ```rust
//! # use omnomnomicon::prelude::*;
//! let p = literal("banana");
//! let q = number::<u32>;
//! let mut c = words((p, q));
//!
//! let r = parse_result(&mut c, "banana 13")?;
//! // ("banana", 13)
//! # assert_eq!(r, ("banana", 13));
//! # Ok::<(), String>(())
//! ```
//!
//! What sets this library apart is that in addition to parsing interactive prompt suggestions and
//! completions are generated:
//!
//! # Example
//! ```rust
//! # use omnomnomicon::prelude::*;
//! let p = literal("banana");
//! let q = literal("pineapple");
//! let c = or(p, q);
//! ```
//!
//! Parser `c` will accept both `"banana"` and `"pineaple"` strings. Given an empty input it will
//! produce [completion][crate::complete] information, given input that starts with either `"b"` or `"p"`
//! - only one completion prompt. Given an input that doesn't start with either of those two
//! prefixes will generate an error message from one of those two parsers.
//!
//! Most of the code in this module is heavily commented so it's a good idea to check the sources.
//! Intended use for this module is to parse one of several available commands and produce a
//! [`Command`] enum, entry point is [`parse_command`].

//! # Functions
//! [`place_cmd`] - Entry point
//!

use crate::prelude::*;

#[derive(Debug, Clone)]
pub enum Command {
    Place(Direction, Option<Item>, u32, Option<u32>),
    Dictionary(usize, u32),
    Sequence,
}

/// Entry point that picks first succeeding parser
///
/// This can be a standalone function or belong to an `impl` of some
/// item in order to provide access to external state, here external state
/// is given as `dict`, see source code. See also [`choice`].
pub fn parse_command(input: &str) -> Result<Command> {
    let dict = &[
        "transform",
        "transformation",
        "transition",
        "translate",
        "transmogrification",
    ];

    choice((
        place_cmd,
        short_place_cmd(),
        perm_cmd,
        dictionary_cmd(dict),
        sequence_cmd,
        mask_cmd,
    ))(input)
}

/// Regular command with help, explicit input and optional elements
///
/// Parsers can provide users with [`help`] messages indicating correct usage.
/// Since [`Item`] is optional - after specifying direction it's possible to specify both
/// price and item. See also [`fmap`].
pub fn place_cmd(input: &str) -> Result<Command> {
    let msg = "Place a buy or sell order for item
Format:

place <bid|ask> <item?> <price> <item> <qty?>

place bid 12 apple # buy 12 apples
place ask 1 potato # sell a single potato";
    let cmd = words((literal("place"), dir, option(item), price, option(qty)));
    let mk = |(_, dir, item, px, qty)| Command::Place(dir, item, px, qty);

    // Note, `input` argument is passed explicitly
    help(msg, fmap(mk, cmd))(input)
}

/// Hidden command with implicit input
///
/// [`hide`] combinator removes completion from dir so user can enter this place command only if he knows of it
///
/// It uses implicit input passing for no reason other than demonstration.
pub fn short_place_cmd() -> impl FnMut(&str) -> Result<Command> {
    let cmd = words((hide(dir), option(item), price, option(qty)));
    let mk = |(dir, item, px, qty)| Command::Place(dir, item, px, qty);

    // Note, `input` argument is absent and replaced with a returned impl Fn(&str) -> Result<R>
    fmap(mk, cmd)
}

/// A command with attributes given in arbitrary order and labels
///
/// One more variant of a place command that accepts arguments in an arbitrary order thanks to
/// [`pwords`] combinator.
///
/// To help _user_ to distinguish between `price` and `qty` they are prefixed with `p` and `q`
/// letters so a valid price would look like `"p12"`. Without this precaution values for price
/// and qty will be assigned arbitrarily.
///
/// Both `perm bid apple p3 q1` `perm p3 q1 apple bid` are valid inputs.
pub fn perm_cmd(input: &str) -> Result<Command> {
    let mk = |(dir, item, px, qty)| Command::Place(dir, item, px, qty);
    // label in price is applied to the decimal number but not to preceeding
    // character 'p' so to provide users with hints we need to reattach this
    // label again, including both literal "p" and numeric "price"
    let tprice = label("price", snd(literal("p"), price));
    let tqty = label("qty", snd(literal("q"), qty));
    let cmd = pwords((dir, option(item), tprice, option(tqty)));
    fmap(mk, snd_word(literal("perm"), cmd))(input)
}

/// A command that depends on external state
///
/// Command takes two parameters - name from a dictionary and a u32 number, number is
/// there to show how to handle finite inputs
///
/// This command also uses implicit input passing which is required because of
/// lack of currying in Rust
pub fn dictionary_cmd(dict: &'static [&'static str]) -> impl FnMut(&str) -> Result<Command> {
    let tup = words((literal("dict"), dict_word(dict), number::<u32>));
    let mk = |(_, ix, c)| Command::Dictionary(ix, c);
    fmap(mk, tup)
}

/// A custom parser with completion info that operates on raw input directly
///
/// A slightly simplified version of [`lookup`] command, parses one of the words from a list
/// given as a parameter and returns it's index on success
pub fn dict_word(dict: &'static [&'static str]) -> impl FnMut(&str) -> Result<usize> {
    move |input| {
        let mut state = State::disabled();
        let mut r = None;

        for (ix, word) in dict.iter().enumerate() {
            let m = check_prefix(word, input);
            // We store full match only when it's at a word boundary:
            // consider dictionary containing both "transform" and "transformation"
            //
            // When input is "transform" - former is the result with no leftovers,
            // " " and "ation" are possible completions.
            //
            // When input is "transforma" there's two possible outcomes:
            // - "transform" as a parse result with leftovers "a"
            // - request for more data with "tion" as a possible continuation
            //
            // Since there's no backtracking parser should disambiguate between
            // those two scenarios
            if m == PrefixCheck::Match && at_word_boundary(word, input) {
                r = Some((ix, word));

            // with partial prefix match we store a potential match
            } else if m == PrefixCheck::Partial {
                state.push(Comp::simple(*word, input.len()));
            }
        }

        // This implementation is simplified to always return whatever information
        // was collected even if it's empty but production code should handle
        // "no completion matches" scenario separately by producing `Terminate::Failure`
        // and `Output(leftovers, None)` accordingly. See [`lookup`] for the right
        // implementation
        match r {
            // no match, hopefully there's some completion results to return
            None => Err(Terminate::from(state)),

            // produce a result and some completion information if we have it
            Some((ix, word)) => {
                let leftovers = &input[word.len()..];
                Ok((Output::new(leftovers, state), ix))
            }
        }
    }
}

/// A sequence of words command - demo for `hint`
///
/// Library is smart enough to compose completion info from separate subparsers
/// into a single _hint_ (not a tab completion) if there's only one path available
pub fn sequence_cmd(input: &str) -> Result<Command> {
    let t = words((
        literal("sequence"),
        literal("of"),
        literal("words"),
        literal("that"),
        literal("shoud"),
        literal("be"),
        literal("completed"),
        literal("all"),
        literal("at"),
        literal("once"),
    ));
    constmap(Command::Sequence, t)(input)
}

/// A command that restricts potential keypresses
///
/// Only digits and spaces are accepted
pub fn mask_cmd(input: &str) -> Result<Command> {
    let p = snd_word(literal("mask"), mask_digits(take_rest));
    fmap(|_| Command::Sequence, p)(input)
}

/// A simple parser with label
pub fn price(input: &str) -> Result<u32> {
    label("price", number::<u32>)(input)
}

/// A simple parser with label and additional restrictions
///
/// 100 is the max accepted qty
pub fn qty(input: &str) -> Result<u32> {
    guard(|x| *x <= 100, label("qty", number::<u32>))(input)
}

#[derive(Debug, Clone)]
pub enum Item {
    Apple,
    Durian,
    Potato,
}

/// Enum parsing: [`choice`] + [`tag`]
///
/// See also [`lookup`] and [`lookup_key`].
pub fn item(input: &str) -> Result<Item> {
    let p = choice((
        tag(Item::Apple, "apple"),
        tag(Item::Durian, "durian"),
        tag(Item::Potato, "potato"),
    ));
    label("item", p)(input)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Direction {
    Bid,
    Ask,
}

/// Enum parsing: [`constmap`] + [`or`]
///
/// There's no performance difference between using [`constmap`] + [`literal`] and [`tag`].
///
/// See also [`lookup`] and [`lookup_key`].
pub fn dir(input: &str) -> Result<Direction> {
    let bid = constmap(Direction::Bid, literal("bid"));
    let ask = constmap(Direction::Ask, literal("ask"));
    label("dir", or(bid, ask))(input)
}
