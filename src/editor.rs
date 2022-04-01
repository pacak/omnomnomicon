//! ## Virtual line editor
//!
//! Features:
//! - consumes keypress events to update internal state with the user input
//! - supports completion and edit history
//!
//! ```
//! # use omnomnomicon::editor::*;
//! let mut edit = LineEdit::default();
//! edit.event(Action::InsertText("hello world"));
//! assert_eq!(edit.view(), "hello world");
//! ```
//!
//! ## Inputs:
//! - [User events](Action) with [`LineEdit::event`]
//! - Edit history: [`LineEdit::push_history`]
//! - Completion info: [`LineEdit::complete_start`]
//!
//! ## Outputs:
//! - Entered text without suggested completion: [`LineEdit::view`]
//! - Suggested completion: [`LineEdit::virt`]
//! - Active completion: [`LineEdit::get_completion`]
//! - Cursor position: [`LineEdit::cursor_pos`]

use std::collections::VecDeque;
use unicode_segmentation::UnicodeSegmentation;

use crate::Comp;

/// Virtual line editor
///
/// Create it with [`Default`] impl, feed with events using [`event`][LineEdit::event],
/// [`complete_start`][LineEdit::complete_start`], [`push_history`][LineEdit::push_history].
///
/// Consume state using [`view`][LineEdit::view], [`preview`][LineEdit::preview],
/// [`virt`][LineEdit::preview]
#[derive(Debug, Default)]
pub struct LineEdit {
    /// Cursor position in bytes.
    ///
    /// Can only point to a "real" text
    byte_cursor: usize,

    /// Cursor position in graphemes.
    ///
    /// Can only point to a "real" text
    grapheme_cursor: usize,

    /// String buffer, actually available info
    ///
    /// contains both "real" and "preview" portions
    buffer: String,

    /// currently active operation. Can be either complete or history search
    operation: Option<Operation>,

    /// list of stored entries, accessible with HistoryPrev/HistoryNext actions
    history: VecDeque<String>,
}

/// Current completion status
///
/// Generate it with [`get_completion`][LineEdit::get_completion]
#[derive(Clone, Debug)]
pub struct Complete {
    /// Matches given to [`LineEdit`] by [`LineEdit::complete_start`]
    pub matches: Vec<String>,

    /// Currently selected match, always a valid index in [`matches`]
    pub current: usize,

    /// matching portion of a string in bytes
    pub matching: usize,

    /// separates "real" and "virtual" portions of a buffer:
    ///
    /// `buffer[0 .. start_of_preview]: "real"`
    /// `buffer[start_of_preview..]: "virtual"
    pub start_of_preview: usize,
}

#[derive(Debug)]
struct HistoryLookup {
    /// line that was in the buffer when history search started, contents
    /// will be lost.
    current_line: String,

    /// currently selected history index, unlike complete search - does not wrap around
    current_index: usize,
}

#[derive(Debug)]
enum Operation {
    Complete(Complete),
    HistorySearch(HistoryLookup),
}

impl LineEdit {
    /// Insert a line into history list
    ///
    /// Keeps up to max entries, earlier values are removed
    pub fn push_history(&mut self, string: String, max: usize) {
        self.history.push_back(string);
        if self.history.len() > max {
            self.history.pop_front();
        }
    }

    fn set_byte_cursor(&mut self, pos: usize) {
        self.byte_cursor = pos;
        self.grapheme_cursor = self.buffer[..self.byte_cursor].graphemes(true).count();
    }

    /// cursor position in characters
    pub fn cursor_pos(&self) -> usize {
        self.grapheme_cursor
    }

    /// editor contents that can be used for parsing, contains only "real" data
    pub fn view(&self) -> &str {
        if let Some(comp) = self.get_completion() {
            &self.buffer[..comp.start_of_preview]
        } else {
            &self.buffer
        }
    }

    /// editor contents with both "real" and "preview" parts
    pub fn preview(&self) -> &str {
        &self.buffer
    }

    /// "preview" part only
    pub fn virt(&self) -> &str {
        if let Some(comp) = self.get_completion() {
            &self.buffer[comp.start_of_preview..]
        } else {
            ""
        }
    }

    /// cancel any ongoing operation, clear the buffers
    pub fn clear(&mut self) {
        self.byte_cursor = 0;
        self.grapheme_cursor = 0;
        self.buffer.clear();
        self.operation = None;
    }

    /// consume new event
    pub fn event(&mut self, event: Action) {
        match event {
            Action::InsertChar(chr) => {
                self.reset_complete();
                if self.byte_cursor == self.buffer.len() {
                    self.buffer.push(chr)
                } else {
                    self.buffer.insert(self.byte_cursor, chr);
                }
                self.byte_cursor += chr.len_utf8();
                self.grapheme_cursor += 1;
            }
            Action::InsertText(string) => {
                self.reset_complete();
                if self.byte_cursor == self.buffer.len() {
                    self.buffer.push_str(string)
                } else {
                    self.buffer.insert_str(self.byte_cursor, string)
                }
                self.byte_cursor += string.len();
                self.grapheme_cursor += string.graphemes(true).count();
            }
            Action::Move(mov) => {
                let new_cursor = self.new_cursor(mov);
                if let Some(comp) = self.get_completion() {
                    if new_cursor > comp.start_of_preview {
                        self.commit_complete();
                        return;
                    }
                }
                self.set_byte_cursor(new_cursor);
            }
            Action::Kill(mov) => {
                self.reset_complete();
                let to = self.new_cursor(mov);
                let range = match to.cmp(&self.byte_cursor) {
                    std::cmp::Ordering::Less => to..self.byte_cursor,
                    std::cmp::Ordering::Equal => return,
                    std::cmp::Ordering::Greater => self.byte_cursor..to,
                };
                self.buffer.replace_range(range.clone(), "");
                self.set_byte_cursor(range.start);
                self.operation = None;
            }
            Action::HistoryPrev => {
                self.reset_complete();
                if let Some(Operation::HistorySearch(hist)) = &mut self.operation {
                    if hist.current_index > 0 {
                        hist.current_index -= 1;
                        self.buffer = self.history[hist.current_index].clone();
                    }
                } else {
                    if self.history.is_empty() {
                        return;
                    }
                    let mut current_line = String::new();
                    std::mem::swap(&mut current_line, &mut self.buffer);
                    let hist = HistoryLookup {
                        current_line,
                        current_index: self.history.len() - 1,
                    };
                    self.buffer = self.history[hist.current_index].clone();
                    self.operation = Some(Operation::HistorySearch(hist));
                }
            }
            Action::HistoryNext => {
                self.reset_complete();
                if let Some(Operation::HistorySearch(hist)) = &mut self.operation {
                    if hist.current_index + 1 == self.history.len() {
                        std::mem::swap(&mut self.buffer, &mut hist.current_line);
                        self.operation = None;
                    } else {
                        hist.current_index += 1;
                        self.buffer = self.history[hist.current_index].clone();
                    }
                }
            }
            Action::CompletePrev => {
                if let Some(Operation::Complete(comp)) = &self.operation {
                    let from = comp.current;
                    let to = (comp.matches.len() + comp.current - 1) % comp.matches.len();
                    self.move_to_completion(from, to);
                }
            }
            Action::CompleteNext => {
                if let Some(Operation::Complete(comp)) = &self.operation {
                    let from = comp.current;
                    let to = (comp.current + 1) % comp.matches.len();
                    self.move_to_completion(from, to);
                }
            }
            Action::Cancel => {
                if let Some(op) = &mut self.operation {
                    match op {
                        Operation::Complete(_) => self.reset_complete(),
                        Operation::HistorySearch(hist) => {
                            std::mem::swap(&mut self.buffer, &mut hist.current_line);
                            self.operation = None;
                        }
                    }
                }
            }
        }
    }

    fn reset_complete(&mut self) {
        if let Some(Operation::Complete(comp)) = &self.operation {
            let start_of_preview = comp.start_of_preview;
            self.set_byte_cursor(start_of_preview);
            self.buffer.truncate(self.byte_cursor);
            self.operation = None;
        }
    }

    fn commit_complete(&mut self) {
        if let Some(Operation::Complete(_comp)) = &self.operation {
            self.set_byte_cursor(self.buffer.len());
            self.operation = None;
        }
    }

    fn move_to_completion(&mut self, from: usize, to: usize) {
        if let Some(Operation::Complete(comp)) = &mut self.operation {
            let to_remove = comp.matches[from].len();
            self.buffer.truncate(self.buffer.len() - to_remove);
            self.buffer.push_str(&comp.matches[to]);
            comp.current = to;
        }
    }

    /// Populate completion info
    pub fn complete_start(&mut self, matches: &[Comp]) {
        // empty inputs disables completion
        let first = match matches.first() {
            Some(first) => first,
            None => {
                self.reset_complete();
                return;
            }
        };

        if let Some(Operation::Complete(comp)) = &mut self.operation {
            if comp
                .matches
                .iter()
                .zip(matches)
                .all(|(a, b)| a == &b.replacement)
            {
                comp.matching = first.remaining;
                return;
            } else {
                self.reset_complete();
            }
        }

        let matching = first.remaining;
        self.buffer.push_str(&first.replacement[matching..]);

        self.operation = Some(Operation::Complete(Complete {
            matches: matches
                .iter()
                .map(|comp| comp.replacement.to_string())
                .collect::<Vec<_>>(),
            matching,
            current: 0,
            start_of_preview: self.byte_cursor,
        }));
    }

    /// gets a reference to current completion, if active
    pub fn get_completion(&self) -> Option<&Complete> {
        if let Some(Operation::Complete(comp)) = &self.operation {
            Some(comp)
        } else {
            None
        }
    }

    fn new_cursor(&self, mov: Move) -> usize {
        // TODO - cache breakdowns?
        match mov {
            Move::BwChar => {
                let mut prev = 0;
                for (ix, _) in self.buffer.grapheme_indices(true) {
                    if ix == self.byte_cursor {
                        return prev;
                    } else {
                        prev = ix;
                    }
                }
                prev
            }
            Move::FwChar => {
                let mut found = false;
                for (ix, _) in self.buffer.grapheme_indices(true) {
                    if found {
                        return ix;
                    }
                    if ix == self.byte_cursor {
                        found = true;
                    }
                }
                self.buffer.len()
            }
            Move::BwWord => {
                let mut prev = 0;
                for (ix, _) in self.buffer.unicode_word_indices() {
                    if ix >= self.byte_cursor {
                        return prev;
                    } else {
                        prev = ix;
                    }
                }
                prev
            }
            Move::FwWord => {
                let mut found = false;
                for (ix, _) in self.buffer.grapheme_indices(true) {
                    if found {
                        return ix;
                    }
                    match ix.cmp(&self.byte_cursor) {
                        std::cmp::Ordering::Less => {}
                        std::cmp::Ordering::Equal => found = true,
                        std::cmp::Ordering::Greater => return ix,
                    }
                }
                self.buffer.len()
            }
            Move::StartOfLine => 0,
            Move::EndOfLine => self.buffer.len(),
        }
    }
}

/// Move cursor relative to current position
#[derive(Debug, Clone, Copy)]
pub enum Move {
    /// back by one unicode grapheme
    BwChar,
    /// back by one unicode word
    BwWord,
    /// forward by one unicode grapheme
    FwChar,
    /// forward by one unicode word
    FwWord,
    /// to the start of the line
    StartOfLine,
    /// to the end of the line
    EndOfLine,
}

#[derive(Debug, Clone)]
/// Next action to perform
pub enum Action<'a> {
    /// Insert character at cursor position
    InsertChar(char),
    /// Insert String at cursor position
    InsertText(&'a str),
    /// move cursor
    Move(Move),
    /// remove everything between old and new cursor positions
    Kill(Move),
    /// cycle though history backwards
    HistoryPrev,
    /// cycle though history forwards
    HistoryNext,
    /// move completion cursor to the previous item, completion info must be set
    CompletePrev,
    /// move completion cursor to the next item, completion info must be set
    CompleteNext,
    /// cancel current operation if any
    Cancel,
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use super::*;

    fn comps(offset: usize, input: &[&'static str]) -> Vec<Comp> {
        input
            .iter()
            .map(|w| Comp {
                replacement: Cow::Borrowed(w),
                display: Cow::Borrowed(w),
                remaining: offset,
            })
            .collect()
    }

    #[test]
    fn unicode_insert_delete() {
        let mut line = LineEdit::default();
        for c in "hello".chars() {
            line.event(Action::InsertChar(c));
        }
        assert_eq!(line.view(), "hello");
        assert_eq!(line.preview(), "hello");

        line.event(Action::Kill(Move::BwWord));
        assert_eq!(line.view(), "");
        for c in "превед".chars() {
            line.event(Action::InsertChar(c));
            line.event(Action::Move(Move::BwChar));
        }
        assert_eq!(line.view(), "деверп");
        assert_eq!(line.preview(), "деверп");

        line.event(Action::Move(Move::EndOfLine));
        line.event(Action::Move(Move::BwChar));
        line.event(Action::Kill(Move::BwChar));
        assert_eq!(line.view(), "девеп");
        assert_eq!(line.preview(), "девеп");

        line.event(Action::Move(Move::EndOfLine));
        line.event(Action::InsertText(" 口水鸡 Phở 비빔밥"));

        line.event(Action::Move(Move::BwWord));
        line.event(Action::Kill(Move::BwWord));
        assert_eq!(line.view(), "девеп 口水鸡 비빔밥");
        assert_eq!(line.preview(), "девеп 口水鸡 비빔밥");

        line.event(Action::Kill(Move::FwChar));
        line.event(Action::Kill(Move::FwChar));
        line.event(Action::Kill(Move::FwChar));
        assert_eq!(line.view(), "девеп 口水鸡 ");
        assert_eq!(line.preview(), "девеп 口水鸡 ");

        line.event(Action::Kill(Move::BwChar));
        assert_eq!(line.view(), "девеп 口水鸡");
        assert_eq!(line.preview(), "девеп 口水鸡");

        line.event(Action::Kill(Move::FwChar));
        assert_eq!(line.view(), "девеп 口水鸡");
        assert_eq!(line.preview(), "девеп 口水鸡");
    }

    #[test]
    fn completion_test() {
        let mut line = LineEdit::default();
        line.event(Action::InsertText("hello w"));

        let words = ["water", "wakanda", "world"];

        line.complete_start(&comps(1, &words));

        assert_eq!(line.preview(), "hello water");
        assert_eq!(line.view(), "hello w");

        line.event(Action::CompleteNext);
        assert_eq!(line.preview(), "hello wakanda");
        assert_eq!(line.view(), "hello w");

        line.event(Action::CompleteNext);
        assert_eq!(line.preview(), "hello world");
        assert_eq!(line.view(), "hello w");

        line.event(Action::CompletePrev);
        assert_eq!(line.preview(), "hello wakanda");
        assert_eq!(line.view(), "hello w");

        line.event(Action::CompleteNext);
        line.event(Action::CompleteNext);
        assert_eq!(line.preview(), "hello water");
        assert_eq!(line.view(), "hello w");

        line.event(Action::CompletePrev);
        assert_eq!(line.preview(), "hello world");
        assert_eq!(line.view(), "hello w");
    }

    #[test]
    fn completion_blank() {
        let mut line = LineEdit::default();
        let words = ["place", "cancel", "potato"];
        line.complete_start(&comps(0, &words));
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "");
        assert_eq!(line.byte_cursor, 0);
        line.event(Action::CompleteNext);

        assert_eq!(line.preview(), "cancel");
        assert_eq!(line.view(), "");
        assert_eq!(line.byte_cursor, 0);
        line.event(Action::CompletePrev);
        line.event(Action::CompletePrev);
        assert_eq!(line.preview(), "potato");
        assert_eq!(line.view(), "");
        assert_eq!(line.byte_cursor, 0);
    }

    #[test]
    fn moving_into_preview_completes_it() {
        let mut line = LineEdit::default();
        let words = ["place"];
        line.complete_start(&comps(0, &words));
        line.event(Action::Move(Move::FwChar));
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "place");
        assert_eq!(line.cursor_pos(), 5);
    }

    #[test]
    fn completion_blank2() {
        let mut line = LineEdit::default();
        let words = ["place"];
        line.complete_start(&comps(0, &words));
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "");
        assert_eq!(line.cursor_pos(), 0);

        line.event(Action::InsertChar('p'));
        line.complete_start(&comps(1, &words));
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "p");
        assert_eq!(line.cursor_pos(), 1);

        line.event(Action::InsertChar('l'));
        line.complete_start(&comps(2, &words));
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "pl");
        assert_eq!(line.cursor_pos(), 2);

        line.event(Action::InsertChar('a'));
        line.complete_start(&comps(3, &words));
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "pla");
        assert_eq!(line.cursor_pos(), 3);

        line.event(Action::Kill(Move::BwChar));
        line.complete_start(&comps(2, &words));
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "pl");
        assert_eq!(line.cursor_pos(), 2);
    }

    #[test]
    fn repeated_comp_start_is_a_noop() {
        let mut line = LineEdit::default();
        let words = ["place"];
        line.event(Action::InsertText("pl"));
        line.complete_start(&comps(2, &words));
        line.complete_start(&comps(2, &words));
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "pl");
        assert_eq!(line.cursor_pos(), 2);
    }

    #[test]
    fn complete_for_second_word() {
        let mut line = LineEdit::default();
        let words = ["potato"];
        line.event(Action::InsertText("place po"));
        line.complete_start(&comps(2, &words));
        line.complete_start(&comps(2, &words));
        assert_eq!(line.preview(), "place potato");
        assert_eq!(line.view(), "place po");
        assert_eq!(line.cursor_pos(), 8);
    }

    #[test]
    fn move_cursor_when_completion_is_active() {
        let mut line = LineEdit::default();
        let words = ["potato"];
        line.event(Action::InsertText("place po"));
        line.complete_start(&comps(2, &words));
        assert_eq!(line.cursor_pos(), 8);
        println!("-{:?}", line);
        line.event(Action::Move(Move::BwChar));
        println!("+{:?}", line);
        assert_eq!(line.cursor_pos(), 7);
        line.event(Action::Move(Move::StartOfLine));
        assert_eq!(line.cursor_pos(), 0);
        line.event(Action::Move(Move::EndOfLine));
        assert_eq!(line.preview(), "place potato");
        assert_eq!(line.view(), "place potato");
        assert_eq!(line.cursor_pos(), 12);
    }

    #[test]
    fn history_works() {
        let mut line = LineEdit::default();
        let history = ["potato", "banana", "apple"];
        for h in &history {
            line.push_history(String::from(*h), 10);
        }

        line.event(Action::HistoryNext);
        assert_eq!(line.preview(), "");
        assert_eq!(line.cursor_pos(), 0);

        line.event(Action::HistoryPrev);
        assert_eq!(line.preview(), history[2]);
        assert_eq!(line.cursor_pos(), 0);

        line.event(Action::HistoryNext);
        assert_eq!(line.preview(), "");
        assert_eq!(line.cursor_pos(), 0);

        line.event(Action::HistoryPrev);
        line.event(Action::HistoryPrev);
        line.event(Action::HistoryPrev);
        assert_eq!(line.preview(), history[0]);
        assert_eq!(line.cursor_pos(), 0);
    }
}
