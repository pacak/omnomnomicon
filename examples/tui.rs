use crossterm::{
    event::{self, Event, KeyCode, KeyModifiers},
    terminal,
};
use omnomnomicon::frontend::tui::*;
use tui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    Terminal,
};

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
    let mut input = ReadlineState::new(&mut p);

    loop {
        terminal.draw(|f| {
            let widget = Readline::new(&input);

            let height = widget.height_hint(20);

            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .constraints([Constraint::Min(1), Constraint::Length(height)])
                .split(f.size());

            let mut cursor = CursorPos::Off;
            f.render_stateful_widget(widget, chunks[1], &mut cursor);

            if let CursorPos::On(pos) = cursor {
                f.set_cursor(pos, chunks[1].y + chunks[1].height)
            }
        })?;

        if !event::poll(std::time::Duration::from_millis(100))? {
            continue;
        }

        if let Some(Event::Key(key)) = input.read(&mut p)? {
            if key.code == KeyCode::Char('c') && key.modifiers == KeyModifiers::CONTROL {
                break;
            } else if key.code == KeyCode::Enter {
                let line = input.editor.view().to_owned();
                // println!("{line}"); <- consume the line here
                input.reset(&mut p);
                input.editor.push_history(line, 100);
            }
        }
    }

    Ok(())
}
