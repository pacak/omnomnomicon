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
            } else if key.code == KeyCode::Enter {
                println!("{:?}", input.editor.view());
            }
        }
    }

    Ok(())
}
