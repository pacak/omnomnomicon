//! Rustyline frontend
//!
//! This module contains a set of functions to connect parser with rustyline library.
use std::sync::{Arc, Mutex, MutexGuard};

use crate::complete::apply_parser_rec;
use crate::*;
use crate::{complete::ParseOutcome, Comp};
use rustyline::{completion::Candidate, hint::Hint};

/// Implements rustyline's [`Hint`]
pub struct RustyHint {
    /// Text to display when hint is active
    pub display: String,
    /// Text to insert in line on the right arrow keypress
    pub replacement: Option<String>,
}

fn colorize(comp: &Comp) -> String {
    use ansi_term::Color;
    format!(
        "{}{}",
        Color::Green.paint(&comp.replacement[0..comp.remaining]),
        Color::Red.paint(&comp.replacement[comp.remaining..])
    )
}

/// Highlight the failing part of the input with red color
///
/// Rustyline allows to highlight the input, thins function will highlight
/// a part the parser can't understand. See [`highlight`][rustyline::highlight::Highlighter::highlight]
///
/// ```ignore
///    // this goes into `Highlighter` trait
///    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
///        match self.cache.peek().as_ref() {
///            Some(ParseOutcome::Failure(x)) => Cow::from(colorize_fail(x, line)),
///            _ => Cow::from(line),
///        }
///    }
/// ```
pub fn colorize_fail(msg: &Failure, line: &str) -> String {
    let split = line.len() - msg.offset;
    let good = &line[..split];
    let bad = &line[split..];
    format!("{}{}", good, ansi_term::Color::Red.paint(bad))
}

impl Candidate for Comp {
    fn display(&self) -> &str {
        &self.display
    }

    fn replacement(&self) -> &str {
        &self.replacement
    }
}

impl Hint for RustyHint {
    fn display(&self) -> &str {
        &self.display
    }

    fn completion(&self) -> Option<&str> {
        self.replacement.as_deref()
    }
}

/// render outcome into something that rustyline can deal with
///
/// rustyline requires to have separate functions for rendering and colorizing prompt,
/// this function creates both versions - with and without color
pub fn render_outcome(res: &ParseOutcome, color: bool) -> RustyHint {
    use ansi_term::Color;
    use std::fmt::Write;
    let mut display = String::new();
    macro_rules! styled {
        ($pat:literal, $style:expr, $value:expr) => {
            if color {
                write!(&mut display, $pat, $style.paint($value)).unwrap()
            } else {
                write!(&mut display, $pat, $value).unwrap()
            }
        };
    }

    let hints = match res {
        ParseOutcome::Hints(hints) => hints,
        ParseOutcome::Failure(msg) => {
            styled!("  {}", Color::Red.reverse(), msg.message.as_ref());
            return RustyHint {
                display,
                replacement: None,
            };
        }
    };

    if let Some(rep) = &hints.replacement {
        styled!("{}", Color::Red, rep)
    }

    if let Some(mask) = &hints.display_mask {
        styled!(" {}", Color::Purple, mask.as_ref())
    }

    if !hints.labels.is_empty() {
        let s = hints.labels.join(" | ");
        styled!(" {}", Color::Green, s);
    };

    if hints.help.is_some() && hints.replacement.is_none() && !hints.help_requested {
        styled!(" {}", Color::Cyan, "?");
    }

    if !hints.comps.is_empty() && hints.replacement.is_none() {
        let variants = hints
            .comps
            .iter()
            .map(|var| {
                if color {
                    colorize(var)
                } else {
                    var.replacement.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join(" | ");
        write!(&mut display, "\n{}", variants).unwrap();
    }

    if let Some(help) = hints.help {
        if hints.help_requested {
            styled!("\n{}", Color::Cyan, help);
        }
    }

    #[cfg(feature = "debug")]
    write!(&mut display, "\n{:?}", hints).unwrap();

    RustyHint {
        display,
        replacement: hints.replacement.clone(),
    }
}

/// Filter a rustyline event using key filters present in the parser
///
/// Implements [`ConditionalEventHandler`][rustyline::ConditionalEventHandler]
pub fn get_event_filter(evt: &rustyline::Event, res: &ParseOutcome) -> Option<rustyline::Cmd> {
    use rustyline::{Cmd, KeyCode, KeyEvent, Modifiers};
    let hints = match res {
        ParseOutcome::Hints(hints) => hints,
        ParseOutcome::Failure(_) => return None,
    };

    if hints.key_mask.is_empty() {
        return None;
    }
    if let KeyEvent(KeyCode::Char(c), m) = evt.get(0)? {
        if m.contains(Modifiers::CTRL) || m.contains(Modifiers::ALT) {
            return None;
        }
        if hints.key_mask.iter().any(|m| m.matches(*c)) {
            None
        } else {
            Some(Cmd::Noop)
        }
    } else {
        None
    }
}

/// Get completion info in `rustyline` friendly format from [`ParseOutcome`]
///
/// Implements [`Completer`][rustyline::completion::Completer]
pub fn get_completion(line: &str, res: Option<&ParseOutcome>) -> (usize, Vec<Comp>) {
    let hints = match res {
        Some(ParseOutcome::Hints(hints)) => hints,
        None | Some(ParseOutcome::Failure(_)) => return (0, Vec::new()),
    };
    let mut offset = 0;
    let mut res = Vec::new();
    for comp in hints.comps.iter() {
        if comp.remaining > offset {
            offset = comp.remaining;
            res.clear();
        }
        res.push(comp.clone());
    }
    (line.len() - offset, res)
}

/// Thread friendly single key cache for ParseOutcome
///
///
#[derive(Debug, Clone)]
pub struct RustylineCache {
    /// key = input
    pub key: Arc<Mutex<String>>,
    /// value = parse outcome, if successful
    pub value: Arc<Mutex<Option<ParseOutcome>>>,
}

impl Default for RustylineCache {
    fn default() -> Self {
        Self {
            key: Arc::new(Mutex::new("Totally not an empty key".to_string())),
            value: Arc::new(Mutex::new(None)),
        }
    }
}

impl RustylineCache {
    /// Try to parse a string using/updating cache
    pub fn get<'a, P, R>(
        &'a self,
        input: &'_ str,
        parser: P,
    ) -> MutexGuard<'a, Option<ParseOutcome>>
    where
        P: Fn(&str) -> Result<R>,
    {
        let mut key = self.key.lock().expect("poisoned mutex");
        let mut value = self.value.lock().expect("poisoned mutex");
        if *key == input {
            return value;
        }
        *key = input.to_string();
        *value = apply_parser_rec(parser, input);
        value
    }

    /// Get a previous stored value, if present
    pub fn peek(&self) -> MutexGuard<Option<ParseOutcome>> {
        self.value.lock().expect("poisoned mutex")
    }
}
