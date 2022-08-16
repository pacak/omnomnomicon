//! Tui frontend
//!
//! This module contains a set of functions to use library in tui library with crossterm frontend

use std::borrow::Cow;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};

use tui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{List, ListItem, ListState, Paragraph, StatefulWidget, Widget},
};

use omnomnomicon::{editor::*, prelude::*};

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
        | KeyCode::Esc
        | KeyCode::CapsLock
        | KeyCode::ScrollLock
        | KeyCode::NumLock
        | KeyCode::PrintScreen
        | KeyCode::Pause
        | KeyCode::Menu
        | KeyCode::KeypadBegin
        | KeyCode::Media(_)
        | KeyCode::Modifier(_) => None,
    }
}

/// readline widget handle, doesn't own anything
pub struct Readline<'a> {
    state: &'a ReadlineState,
}

impl<'a> Readline<'a> {
    /// constructor
    pub fn new(state: &'a ReadlineState) -> Self {
        Self { state }
    }
}

impl<'a> Readline<'a> {
    /// get recommended height
    pub fn height_hint(&self, max: usize) -> u16 {
        if let Some(help) = self.state.outcome.help() {
            help.lines().count().min(max) as u16
        } else if let Some(comp) = self.state.outcome.completions() {
            if comp.iter().all(|i| i.is_simple()) {
                2
            } else {
                comp.len().min(max) as u16 + 1
            }
        } else {
            1
        }
    }
}

struct CompsBlock<'a> {
    complete: &'a Complete,
    items: &'a [Comp],
}

impl Widget for CompsBlock<'_> {
    fn render(self, area: Rect, buf: &mut tui::buffer::Buffer) {
        if self.items.iter().all(|i| i.is_simple()) {
            let mut spans = Vec::with_capacity(self.items.len() * 3 + 1);
            for (ix, comp) in self.items.iter().enumerate() {
                let base = if ix == self.complete.current && self.items.len() > 1 {
                    spans.push(Span::raw("> "));
                    Style::default().add_modifier(Modifier::BOLD)
                } else {
                    Style::default()
                };
                spans.push(Span::styled(
                    &comp.replacement[..comp.remaining],
                    base.fg(Color::Green),
                ));
                spans.push(Span::styled(
                    &comp.replacement[comp.remaining..],
                    base.fg(Color::Red),
                ));
                if ix == self.complete.current && self.items.len() > 1 {
                    spans.push(Span::raw(" <"));
                }
                spans.push(Span::raw(" | "));
            }
            spans.pop();
            Paragraph::new(Spans::from(spans)).render(area, buf);
        } else {
            let items = self
                .items
                .iter()
                .map(|comp| {
                    ListItem::new(comp.display.clone()).style(Style::default().fg(Color::Red))
                })
                .collect::<Vec<_>>();

            let list = List::new(items)
                .style(Style::default().fg(Color::White))
                .highlight_style(Style::default().add_modifier(Modifier::ITALIC))
                .highlight_symbol("> ");
            let mut state = ListState::default();
            state.select(Some(self.complete.current));

            StatefulWidget::render(list, area, buf, &mut state);
        }
    }
}

impl StatefulWidget for Readline<'_> {
    type State = CursorPos;
    fn render(self, area: Rect, buf: &mut tui::buffer::Buffer, cursor: &mut CursorPos) {
        let state = self.state;
        let prompt = "> ";

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(area.height - 1), // help or variants
                Constraint::Length(1),               // input line itself
            ])
            .split(area);
        let extra_rect = chunks[0];
        let input_rect = chunks[1];
        let cursor_pos = (state.editor.cursor_pos() + prompt.chars().count()) as u16 + area.x;

        match &state.outcome {
            ParseOutcome::Hints(h) => {
                if let Some(help) = h.help() {
                    let text = Text::styled(help, Style::default().fg(Color::Cyan));
                    Paragraph::new(text).render(area, buf);
                    *cursor = CursorPos::Off;
                } else {
                    if let Some(complete) = state.editor.get_completion() {
                        CompsBlock {
                            complete,
                            items: &h.comps,
                        }
                        .render(extra_rect, buf);
                    }
                    InputLine {
                        prompt,
                        input: state.editor.view(),
                        virt: state.editor.virt(),
                        labels: &h.labels,
                        help_available: h.help.is_some(),
                        failure: None,
                        cursor_pos,
                    }
                    .render(input_rect, buf, cursor);
                }
            }
            ParseOutcome::Failure(f) => InputLine {
                prompt,
                input: state.editor.preview(),
                virt: "",
                labels: &[],
                failure: Some(f),
                help_available: false,
                cursor_pos,
            }
            .render(input_rect, buf, cursor),
            ParseOutcome::Success => InputLine {
                prompt,
                input: state.editor.preview(),
                virt: "",
                labels: &[],
                failure: None,
                help_available: false,
                cursor_pos,
            }
            .render(input_rect, buf, cursor),
        }
    }
}

/// Cursor horizontal position
#[derive(Debug, Copy, Clone)]
pub enum CursorPos {
    /// don't display cursor - we are probably showing help
    Off,

    /// show cursor at this X offset
    On(u16),
}

impl StatefulWidget for InputLine<'_> {
    type State = CursorPos;

    fn render(self, area: Rect, buf: &mut tui::buffer::Buffer, cursor: &mut CursorPos) {
        let mut labels = self.labels.to_vec();
        labels.sort();
        labels.dedup();

        let mut spans = Vec::with_capacity(2 + labels.len() * 2 + 4);

        spans.push(Span::raw(self.prompt));
        match self.failure {
            Some(f) => {
                spans.push(Span::from(f.good_part_of_input(self.input)));
                spans.push(Span::styled(
                    f.bad_part_of_input(self.input),
                    Style::default().bg(Color::Red),
                ));
                spans.push(Span::from(" "));
                spans.push(Span::styled(
                    f.message.clone(),
                    Style::default().fg(Color::Red),
                ));
            }
            None => {
                spans.push(Span::raw(self.input));
                if !self.virt.is_empty() {
                    spans.push(Span::styled(self.virt, Style::default().fg(Color::Red)));
                }

                if self.help_available {
                    spans.push(Span::styled(" ?", Style::default().fg(Color::Cyan)));
                }

                if !labels.is_empty() {
                    spans.push(Span::raw(" "));
                    for label in labels.into_iter() {
                        spans.push(Span::styled(label, Style::default().fg(Color::Green)));
                        spans.push(Span::raw(" | "));
                    }
                    spans.pop();
                }
            }
        }
        let text = Text::from(Spans::from(spans));
        let width = text.width() as u16;
        if width > area.width {
            let offset = width - area.width;
            *cursor = CursorPos::On(self.cursor_pos - offset);
            Paragraph::new(text).scroll((0, offset)).render(area, buf);
        } else {
            *cursor = CursorPos::On(self.cursor_pos);
            Paragraph::new(text).render(area, buf);
        }
    }
}

struct InputLine<'a> {
    prompt: &'static str,
    input: &'a str,
    virt: &'a str,
    labels: &'a [Cow<'static, str>],
    failure: Option<&'a Failure>,
    help_available: bool,
    cursor_pos: u16,
}

#[derive(Debug)]
/// readline-like widget for tui
pub struct ReadlineState {
    /// line edit instance
    pub editor: LineEdit,
    outcome: ParseOutcome,
}

impl ReadlineState {
    /// generate new parser instance
    pub fn new<P, R>(parser: P) -> Self
    where
        P: FnMut(&str) -> omnomnomicon::Result<R>,
    {
        let outcome = apply_parser(parser, "");

        let mut editor = LineEdit::default();
        if let Some(comp) = outcome.completions() {
            editor.complete_start(comp);
        }
        Self { editor, outcome }
    }

    /// wipe state, refresh rendering
    pub fn reset<P, R>(&mut self, parser: P)
    where
        P: FnMut(&str) -> omnomnomicon::Result<R>,
    {
        self.outcome = apply_parser(parser, "");
        self.editor.clear();
        if let Some(comp) = self.outcome.completions() {
            self.editor.complete_start(comp);
        }
    }

    /// read key, handle or return it
    pub fn read<R, P>(&mut self, parser: P) -> crossterm::Result<Option<Event>>
    where
        P: FnMut(&str) -> omnomnomicon::Result<R>,
    {
        let evt = event::read()?;
        let key = match evt {
            Event::Key(key) => key,
            _ => return Ok(Some(evt)),
        };

        if let Some(action) = keycode_to_action(key) {
            self.editor.event(action);
            self.outcome = apply_parser(parser, self.editor.view());

            if let Some(comp) = self.outcome.completions() {
                self.editor.complete_start(comp);
            }

            Ok(None)
        } else {
            Ok(Some(evt))
        }
    }
}
