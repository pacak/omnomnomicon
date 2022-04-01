//! Tui frontend
//!
//! This module contains a set of functions to use library in tui library with crossterm frontend

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};

use tui::{
    backend::Backend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::{Span, Spans, Text},
    widgets::Paragraph,
    Frame,
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
        | KeyCode::Esc => None,
    }
}

#[derive(Debug)]
/// readline-like widget for tui
pub struct Readline {
    /// line edit instance
    pub editor: LineEdit,
    outcome: ParseOutcome,
}

fn render_completions<B: Backend>(f: &mut Frame<B>, rect: Rect, comps: &[Comp]) {
    let mut spans = comps
        .iter()
        .flat_map(|c| {
            [
                Span::styled(
                    &c.replacement[..c.remaining],
                    Style::default().fg(Color::Green),
                ),
                Span::styled(
                    &c.replacement[c.remaining..],
                    Style::default().fg(Color::Red),
                ),
                Span::raw(" | "),
            ]
        })
        .collect::<Vec<_>>();
    spans.pop();
    let p = Paragraph::new(Spans::from(spans));
    f.render_widget(p, rect);
}

fn render_completions_large<B: Backend>(
    f: &mut Frame<B>,
    rect: Rect,
    comps: &[Comp],
    current: usize,
) {
    let start = usize::saturating_sub(current, rect.height as usize / 5);
    let end = usize::min(start + comps.len(), start + rect.height as usize).min(comps.len());

    let mut lines = Vec::new();

    for (ix, comp) in comps.iter().enumerate().take(end).skip(start) {
        let mut spans = Vec::new();
        if ix == current {
            spans.push(Span::raw("> "));
        } else {
            spans.push(Span::raw("  "));
        }
        spans.push(Span::raw(comp.display.clone()));
        lines.push(Spans::from(spans));
    }

    let p = Paragraph::new(lines);
    f.render_widget(p, rect);
}

impl Readline {
    /// generate new parser instance
    pub fn new<P, R>(parser: P) -> Self
    where
        P: FnMut(&str) -> omnomnomicon::Result<R>,
    {
        let outcome = apply_parser_rec(parser, "");
        Self {
            editor: LineEdit::default(),
            outcome,
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
            self.outcome = apply_parser_rec(parser, self.editor.view());
            Ok(None)
        } else {
            Ok(Some(evt))
        }
    }

    /// render widget into a rectagle, rect should be at least 12 lines tall
    pub fn render<B: Backend>(&mut self, f: &mut Frame<B>, rect: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(10), // help or variants
                Constraint::Min(1),     // command specific help such as baikai
                Constraint::Length(1),  // input line itself
            ])
            .split(rect);

        // contains help or visible completion variants
        let meta_rect = chunks[0];
        let variants_rect = chunks[1];
        let input_rect = chunks[2];

        let prompt = "> ";

        if let Some(comp) = self.outcome.completions() {
            self.editor.complete_start(comp);
        }

        match &self.outcome {
            ParseOutcome::Hints(hints) => {
                if let Some(help) = hints.help() {
                    let w = Text::styled(help, Style::default().fg(Color::Cyan));
                    f.render_widget(Paragraph::new(w), meta_rect);
                } else if let Some(comp) = self.editor.get_completion() {
                    if !hints.comps.is_empty() {
                        render_completions_large(f, meta_rect, &hints.comps, comp.current);
                    }
                }

                render_completions(f, variants_rect, &hints.comps);

                let label = if hints.labels.is_empty() {
                    Span::raw("")
                } else {
                    let l = format!(" {}", hints.labels.join(" | "));
                    Span::styled(l, Style::default().fg(Color::Green))
                };

                let input = Paragraph::new(Spans::from(vec![
                    Span::raw(prompt),
                    Span::from(self.editor.view()),
                    Span::styled(self.editor.virt(), Style::default().fg(Color::Red)),
                    label,
                ]));
                f.render_widget(input, input_rect);
            }
            // parse failed due to invalid input
            ParseOutcome::Failure(failure) => {
                let input = self.editor.preview();
                let input_widget = Paragraph::new(Spans::from(vec![
                    Span::raw(prompt),
                    Span::from(failure.good_part_of_input(input)),
                    Span::styled(
                        failure.bad_part_of_input(input),
                        Style::default().bg(Color::Red),
                    ),
                    Span::from(" "),
                    Span::styled(failure.message.clone(), Style::default().fg(Color::Red)),
                ]));
                f.render_widget(input_widget, input_rect);
            }
            // Parse succeeded, not much we can do here
            ParseOutcome::Success => {
                let input_widget = Paragraph::new(Spans::from(vec![
                    Span::raw(prompt),
                    Span::from(self.editor.preview()),
                ]));
                f.render_widget(input_widget, input_rect);
            }
        }

        // Make the cursor visible and ask tui-rs to put it at the specified coordinates after rendering
        f.set_cursor(
            input_rect.x + self.editor.cursor_pos() as u16 + prompt.chars().count() as u16,
            input_rect.y,
        )
    }
}
