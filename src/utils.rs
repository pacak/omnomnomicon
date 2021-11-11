//! A collection of non parser things that's still useful to write parsers

/// Produced by [`check_prefix`]
#[derive(Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Debug)]
pub enum PrefixCheck {
    /// `input` contains a complete copy of `prefix` and maybe more data
    Match,
    /// `input` starts with `prefix`
    Partial,
    /// `input` starts with something else
    Mismatch,
}

/// Check if string can possibly contain a pattern
///
/// Parsing a static string off input can result in 3 possible scenarios:
/// - input contains a string, eg: "banana cake" contains "banana" so it's a match
/// - input is a prefix of string, eg: "ban" is a prefix of "banana" so a partial match
/// - input contains something else so there's no match.
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let pat = "banana";
///
/// let r = check_prefix(pat, "banana cake");
/// // Match
/// # assert_eq!(r, PrefixCheck::Match);
///
/// let r = check_prefix(pat, "ban");
/// // Partial
/// # assert_eq!(r, PrefixCheck::Partial);
///
/// let r = check_prefix(pat, "hotpot");
/// // Mismatch
/// # assert_eq!(r, PrefixCheck::Mismatch);
/// ```
#[inline]
pub fn check_prefix(pattern: &str, input: &str) -> PrefixCheck {
    match pattern.len().cmp(&input.len()) {
        std::cmp::Ordering::Less | std::cmp::Ordering::Equal => {
            if input.starts_with(pattern) {
                PrefixCheck::Match
            } else {
                PrefixCheck::Mismatch
            }
        }
        std::cmp::Ordering::Greater => {
            if pattern.starts_with(input) {
                PrefixCheck::Partial
            } else {
                PrefixCheck::Mismatch
            }
        }
    }
}

/// Check if pattern location is at a word boundary of input
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let pat = "ban";
/// assert!(at_word_boundary(pat, "ban"));
/// assert!(at_word_boundary(pat, "ban on a server"));
/// assert!(!at_word_boundary(pat, "banana"));
pub fn at_word_boundary(pattern: &str, input: &str) -> bool {
    input.len() == pattern.len() || input[pattern.len()..].starts_with(' ')
}
