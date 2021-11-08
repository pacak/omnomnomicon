use std::{
    borrow::Cow,
    ops::Deref,
    sync::{Arc, Mutex, MutexGuard},
};

use omnomnomicon::{frontend::rustyline::*, prelude::*, tutorial::*};
use rustyline::{
    completion::Completer, highlight::Highlighter, hint::Hinter, validate::Validator,
    CompletionType, ConditionalEventHandler, Event, EventHandler, Helper,
};

#[derive(Debug, Clone)]
struct H {
    parsed_key: String,
    parsed_value: Option<ParseOutcome>,
}

impl H {
    fn parse(input: &str) -> Result<Command> {
        parse_command(input)
    }

    fn new() -> Self {
        let parsed_key = "".to_owned();
        let parsed_value = apply_parser_rec(parse_command, &parsed_key);
        Self {
            parsed_key,
            parsed_value,
        }
    }

    fn cached(&mut self, input: &str) -> Option<&ParseOutcome> {
        if self.parsed_key == input {
            return self.parsed_value.as_ref();
        }
        self.parsed_key = input.to_owned();
        self.parsed_value = apply_parser_rec(parse_command, input);
        self.parsed_value.as_ref()
    }
}

impl Completer for M {
    type Candidate = Comp;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let input = &line[..pos];
        Ok(get_completion(input, self.0.lock().unwrap().cached(input)))
    }
}

impl ConditionalEventHandler for M {
    fn handle(
        &self,
        evt: &Event,
        _n: rustyline::RepeatCount,
        _positive: bool,
        ctx: &rustyline::EventContext,
    ) -> Option<rustyline::Cmd> {
        let input = &ctx.line()[..ctx.pos()];
        get_event_filter(evt, self.0.lock().unwrap().cached(&input).as_ref()?)
    }
}

impl Helper for M {}
impl Validator for M {}
impl Highlighter for M {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        match self.0.lock().unwrap().parsed_value.as_ref() {
            Some(res) => {
                let r = render_outcome(&res, true);
                Cow::from(r.display)
            }
            None => Cow::from(hint),
        }
    }

    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        match self.0.lock().unwrap().parsed_value.as_ref() {
            Some(ParseOutcome::Failure(x)) => Cow::from(colorize_fail(x, line)),
            _ => Cow::from(line),
        }
    }
}

impl Hinter for M {
    type Hint = RustyHint;
    fn hint(&self, line: &str, pos: usize, _ctx: &rustyline::Context<'_>) -> Option<Self::Hint> {
        let input = &line[..pos];
        Some(render_outcome(
            self.0.lock().unwrap().cached(input).as_ref()?,
            false,
        ))
    }
}

#[derive(Clone)]
struct M(Arc<Mutex<H>>);

fn main() {
    let cfg = rustyline::Config::builder()
        .completion_type(CompletionType::Circular)
        .build();

    let helper = M(Arc::from(Mutex::from(H::new())));
    let mut rl = rustyline::Editor::<M>::with_config(cfg);
    rl.set_helper(Some(helper.clone()));
    rl.bind_sequence(
        Event::Any,
        EventHandler::Conditional(Box::new(helper.clone())),
    );
    while let Ok(line) = rl.readline(">> ") {
        let parsed = H::parse(&line);
        println!("{:?}", parsed);
    }
}
