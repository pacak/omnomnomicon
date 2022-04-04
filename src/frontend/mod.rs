//! # Frontends
//!
//! Library implements parsing and completion logic independently from user interaction logic.
//! Frontend is something that implements user interaction parts.
//!
//! Recomended frontend is [`tui`]
//!
//! # Making custom frontend with help of [`LineEdit`][crate::editor::LineEdit]
//!
//! 1. Initialize `LineEdit`
//! ```no_run
//! # use omnomnomicon::editor::*;
//! let mut edit = LineEdit::default();
//! ```
//!
//! 2. Start capturing user events and converting them into editor
//!    [`Action`](crate::editor::Action) using something similar to
//!    [`keycode_to_action`](crate::frontend::tui::keycode_to_action)
//!    and feeding them to the editor in a loop
//!
//! ```ignore
//! loop {
//!    ...
//!    let event = capture();
//!    if let Some(event) = keycode_to_action(event) {
//!        edit.event(action);
//!    }
//!    ...
//! }
//! ```
//!
//! 3. Inside the same loop extract a current value from the editor and get parse outcome with
//!    `apply_parser`, update editor state with completion info, if available
//!
//! ```ignore
//! loop {
//!    ...
//!    let parser = ...
//!    let outcome = apply_parser(parser, edit.view())
//!
//!    match outcome {
//!        ParseOutcome::Hints(hints) => {
//!            if let Some(comp) = edit.get_completion() {
//!                edit.complete_start(comp);
//!            }
//!
//!        }
//!    }
//! }
//! ```
//!
//! 4. Inside the same loop render outcome and various internal states, completion state, etc
//!
//! 5. To extract the value itself if user event corresponds to Enter call parser directly.
//!    You can check that whole input is consumed by checking `output.input.is_empty()`
//! ```ignore
//!    ...
//!    if let Ok((output, result)) = parser(editor.view()) {
//!        ...
//!    }
//!    ...
//! ```
//!
//! 6. After that it's probably a good idea to clean the editor and save the input to editor
//!    history:
//! ```ignore
//!    ...
//!    let line = edit.view().to_owned();
//!    input.editor.clear();
//!    input.editor.push_history(line, 100);
//!    ...
//! ```

#[cfg(feature = "tui")]
pub mod tui;
