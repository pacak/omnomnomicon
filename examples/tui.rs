use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::{Span, Spans, Text},
    widgets::Paragraph,
    Frame, Terminal,
};

use crossterm::{
    event::{self, Event, KeyCode, KeyModifiers},
    terminal,
};
use omnomnomicon::frontend::tui::*;
use omnomnomicon::{editor::*, prelude::*};

#[derive(Debug)]
struct Readline {
    editor: LineEdit,
    outcome: ParseOutcome,
    pub replacement: Option<String>,
}

pub fn render_completions<B: Backend>(f: &mut Frame<B>, rect: Rect, comps: &[Comp]) {
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

pub fn render_completions_large<B: Backend>(
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
    pub fn new<P, R>(parser: P) -> Self
    where
        P: FnMut(&str) -> omnomnomicon::Result<R>,
    {
        let outcome = apply_parser_rec(parser, "");
        Self {
            editor: LineEdit::default(),
            outcome,
            replacement: None,
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
            let matching = comp[0].remaining;
            let matches = comp
                .iter()
                .map(|c| c.replacement.as_ref())
                .collect::<Vec<_>>();
            self.editor.complete_start(matching, &matches);
        }

        self.replacement = None;
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
                self.replacement = hints.replacement.clone();
                let hint = hints
                    .replacement
                    .as_ref()
                    .map_or_else(String::new, |x| x.clone());

                let label = if hints.labels.is_empty() {
                    Span::raw("")
                } else {
                    let l = format!(" {}", hints.labels.join(" | "));
                    Span::styled(l, Style::default().fg(Color::Green))
                };

                let input = Paragraph::new(Spans::from(vec![
                    Span::raw(prompt),
                    Span::from(self.editor.preview()),
                    Span::styled(hint, Style::default().fg(Color::Red)),
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

struct RawModeGuard;
impl RawModeGuard {
    pub fn new() -> std::io::Result<Self> {
        terminal::enable_raw_mode()?;
        Ok(RawModeGuard)
    }
}

impl Drop for RawModeGuard {
    fn drop(&mut self) {
        terminal::disable_raw_mode().unwrap();
    }
}

fn main() -> std::io::Result<()> {
    let mut stdout = std::io::stdout();
    let _g = RawModeGuard::new()?;

    crossterm::execute!(stdout, terminal::Clear(terminal::ClearType::All),)?;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let mut p = omnomnomicon::tutorial::parse_command;
    let mut input = Readline::new(&mut p);

    loop {
        terminal.draw(|f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Min(1), Constraint::Length(12)])
                .split(f.size());

            input.render(f, chunks[1]);
        })?;

        if !event::poll(std::time::Duration::from_millis(100))? {
            continue;
        }

        if let Some(Event::Key(key)) = input.read(&mut p)? {
            if key.code == KeyCode::Char('c') && key.modifiers == KeyModifiers::CONTROL {
                break;
            }
        }
    }

    Ok(())
}
