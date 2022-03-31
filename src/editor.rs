//! virtual line editor
//!
//! features:
//! - consumes keypress events and updates internal state with the user input
//! - supports completion and edit history

use std::collections::VecDeque;
use unicode_segmentation::UnicodeSegmentation;

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
    cursor: usize,

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
    /// Currently valid matches
    pub matches: Vec<String>,

    /// currently selected match, always a valid index in [`matches`]
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

    /// cursor position in characters
    pub fn cursor_pos(&self) -> usize {
        // TODO - cache graphemes
        self.buffer[..self.cursor].graphemes(true).count()
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
        self.cursor = 0;
        self.buffer.clear();
        self.operation = None;
    }

    /// consume new event
    pub fn event(&mut self, event: Action) {
        match event {
            Action::InsertChar(chr) => {
                self.reset_complete();
                if self.cursor == self.buffer.len() {
                    self.buffer.push(chr)
                } else {
                    self.buffer.insert(self.cursor, chr);
                }
                self.cursor += chr.len_utf8();
            }
            Action::InsertText(string) => {
                self.reset_complete();
                if self.cursor == self.buffer.len() {
                    self.buffer.push_str(string)
                } else {
                    self.buffer.insert_str(self.cursor, string)
                }
                self.cursor += string.len();
            }
            Action::Move(mov) => {
                let new_cursor = self.new_cursor(mov);
                if let Some(comp) = self.get_completion() {
                    if new_cursor > comp.start_of_preview {
                        self.commit_complete();
                        return;
                    }
                }
                self.cursor = new_cursor;
            }
            Action::Kill(mov) => {
                self.reset_complete();
                let to = self.new_cursor(mov);
                let range = match to.cmp(&self.cursor) {
                    std::cmp::Ordering::Less => to..self.cursor,
                    std::cmp::Ordering::Equal => return,
                    std::cmp::Ordering::Greater => self.cursor..to,
                };
                self.buffer.replace_range(range.clone(), "");
                self.cursor = range.start;
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
            self.cursor = comp.start_of_preview;
            self.buffer.truncate(self.cursor);
            self.operation = None;
        }
    }

    fn commit_complete(&mut self) {
        if let Some(Operation::Complete(_comp)) = &self.operation {
            self.cursor = self.buffer.len();
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

    /// Populate completion variants
    ///
    /// matching - length in bytes of a matching portion. `matching` bytes
    /// at the end of the real buffer part of the buffer should be
    /// the same as `matching` bytes of every string inside matches.
    ///
    /// matches must be non empty
    pub fn complete_start(&mut self, matching: usize, matches: &[&str]) {
        if let Some(Operation::Complete(comp)) = &mut self.operation {
            if comp.matches == matches {
                comp.matching = matching;
            }
            return;
        }
        let first = matches.first().expect("complete start with no variants?");
        self.buffer.push_str(&first[matching..]);

        self.operation = Some(Operation::Complete(Complete {
            matches: matches
                .iter()
                .copied()
                .map(String::from)
                .collect::<Vec<_>>(),
            matching,
            current: 0,
            start_of_preview: self.cursor,
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
        // TODO - cache breakdowns
        match mov {
            Move::BwChar => {
                let mut prev = 0;
                for (ix, _) in self.buffer.grapheme_indices(true) {
                    if ix == self.cursor {
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
                    if ix == self.cursor {
                        found = true;
                    }
                }
                self.buffer.len()
            }
            Move::BwWord => {
                let mut prev = 0;
                for (ix, _) in self.buffer.unicode_word_indices() {
                    if ix >= self.cursor {
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
                    match ix.cmp(&self.cursor) {
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
    use super::*;

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

        line.complete_start(1, &words);

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
        line.complete_start(0, &words);
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "");
        assert_eq!(line.cursor, 0);
        line.event(Action::CompleteNext);

        assert_eq!(line.preview(), "cancel");
        assert_eq!(line.view(), "");
        assert_eq!(line.cursor, 0);
        line.event(Action::CompletePrev);
        line.event(Action::CompletePrev);
        assert_eq!(line.preview(), "potato");
        assert_eq!(line.view(), "");
        assert_eq!(line.cursor, 0);
    }

    #[test]
    fn moving_into_preview_completes_it() {
        let mut line = LineEdit::default();
        let words = ["place"];
        line.complete_start(0, &words);
        line.event(Action::Move(Move::FwChar));
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "place");
        assert_eq!(line.cursor_pos(), 5);
    }

    #[test]
    fn completion_blank2() {
        let mut line = LineEdit::default();
        let words = ["place"];
        line.complete_start(0, &words);
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "");
        assert_eq!(line.cursor_pos(), 0);

        line.event(Action::InsertChar('p'));
        line.complete_start(1, &words);
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "p");
        assert_eq!(line.cursor_pos(), 1);

        line.event(Action::InsertChar('l'));
        line.complete_start(2, &words);
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "pl");
        assert_eq!(line.cursor_pos(), 2);

        line.event(Action::InsertChar('a'));
        line.complete_start(3, &words);
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "pla");
        assert_eq!(line.cursor_pos(), 3);

        line.event(Action::Kill(Move::BwChar));
        line.complete_start(2, &words);
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "pl");
        assert_eq!(line.cursor_pos(), 2);
    }

    #[test]
    fn repeated_comp_start_is_a_noop() {
        let mut line = LineEdit::default();
        let words = ["place"];
        line.event(Action::InsertText("pl"));
        line.complete_start(2, &words);
        line.complete_start(2, &words);
        assert_eq!(line.preview(), "place");
        assert_eq!(line.view(), "pl");
        assert_eq!(line.cursor_pos(), 2);
    }

    #[test]
    fn complete_for_second_word() {
        let mut line = LineEdit::default();
        let words = ["potato"];
        line.event(Action::InsertText("place po"));
        line.complete_start(2, &words);
        line.complete_start(2, &words);
        assert_eq!(line.preview(), "place potato");
        assert_eq!(line.view(), "place po");
        assert_eq!(line.cursor_pos(), 8);
    }

    #[test]
    fn move_cursor_when_completion_is_active() {
        let mut line = LineEdit::default();
        let words = ["potato"];
        line.event(Action::InsertText("place po"));
        line.complete_start(2, &words);
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
