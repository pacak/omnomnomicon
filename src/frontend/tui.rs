//! Tui frontend
//!
//! This module contains a set of functions to use library in tui library with crossterm frontend

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::editor::*;

/// Keystroke     | Action
/// ---------     | ------
/// Ctrl-A, Home  | Move cursor to the beginning of the line
/// Ctrl-E, End   | Move cursor to the end of the line
/// Ctrl-B, Left  | Move cursor one grapheme to the left
/// Ctrl-F, Right | Move cursor one grapheme to the right
/// Ctrl-H, Backspace | Delete the grapheme to the left of the cursor
/// Delete        | Delete the grapheme to the right of the cursor
/// Ctrl-J, Ctrl-M, Enter | Finish line editing and accept the current line
/// Ctrl-K        | Delete from cursor to end of line
/// Ctrl-W        | Delete word leading up to cursor
/// Alt-b, Alt-Left | Move the cursor backwards one word
/// Alt-f, Alt-Right | Move the cursor forwards one word
/// Tab | Switch to the next completion variant
/// Shift-Tab | Switch to the previous completion variant
pub fn keycode_to_action(key: KeyEvent) -> Option<Action<'static>> {
    match key.code {
        KeyCode::Backspace => Some(Action::Kill(Move::BwChar)),
        KeyCode::Left => {
            if key.modifiers.contains(KeyModifiers::ALT) {
                Some(Action::Move(Move::BwWord))
            } else {
                Some(Action::Move(Move::BwChar))
            }
        }
        KeyCode::Right => {
            if key.modifiers.contains(KeyModifiers::ALT) {
                Some(Action::Move(Move::FwWord))
            } else {
                Some(Action::Move(Move::FwChar))
            }
        }
        KeyCode::Up => Some(Action::HistoryPrev),
        KeyCode::Down => Some(Action::HistoryNext),
        KeyCode::Home => Some(Action::Move(Move::StartOfLine)),
        KeyCode::End => Some(Action::Move(Move::EndOfLine)),
        KeyCode::Tab => Some(Action::CompleteNext),
        KeyCode::BackTab => Some(Action::CompletePrev),
        KeyCode::Delete => Some(Action::Kill(Move::FwChar)),
        KeyCode::Char(c) => {
            if c == 'c' && key.modifiers.contains(KeyModifiers::CONTROL) {
                None
            } else if c == 'a' && key.modifiers.contains(KeyModifiers::CONTROL) {
                Some(Action::Move(Move::StartOfLine))
            } else if c == 'b' && key.modifiers.contains(KeyModifiers::CONTROL) {
                Some(Action::Move(Move::BwChar))
            } else if c == 'f' && key.modifiers.contains(KeyModifiers::CONTROL) {
                Some(Action::Move(Move::FwChar))
            } else if c == 'b' && key.modifiers.contains(KeyModifiers::ALT) {
                Some(Action::Move(Move::BwWord))
            } else if c == 'f' && key.modifiers.contains(KeyModifiers::ALT) {
                Some(Action::Move(Move::FwWord))
            } else if c == 'h' && key.modifiers.contains(KeyModifiers::CONTROL) {
                Some(Action::Kill(Move::BwChar))
            } else if c == 'e' && key.modifiers.contains(KeyModifiers::CONTROL) {
                Some(Action::Move(Move::EndOfLine))
            } else if c == 'k' && key.modifiers.contains(KeyModifiers::CONTROL) {
                Some(Action::Kill(Move::EndOfLine))
            } else if c == 'w' && key.modifiers.contains(KeyModifiers::CONTROL) {
                Some(Action::Kill(Move::BwWord))
            } else {
                Some(Action::InsertChar(c))
            }
        }
        KeyCode::Enter
        | KeyCode::F(_)
        | KeyCode::PageUp
        | KeyCode::PageDown
        | KeyCode::Insert
        | KeyCode::Null
        | KeyCode::Esc => None,
    }
}
