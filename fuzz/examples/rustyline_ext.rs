use std::{borrow::Cow, sync::MutexGuard};

use omnomnomicon::{frontend::rustyline::*, prelude::*};
use omnomnomicon_tests::{parse_ext, CommandExt};
use rustyline::{
    completion::Completer, highlight::Highlighter, hint::Hinter, validate::Validator,
    CompletionType, ConditionalEventHandler, Event, EventHandler, Helper,
};

#[derive(Debug, Clone)]
struct H {
    cache: RustylineCache,
}

impl H {
    fn parse(input: &str) -> Result<CommandExt> {
        parse_ext(input)
    }

    fn new() -> Self {
        Self {
            cache: RustylineCache::default(),
        }
    }

    fn cached(&self, input: &str) -> MutexGuard<'_, Option<ParseOutcome>> {
        self.cache.get(input, Self::parse)
    }
}

impl Completer for H {
    type Candidate = Comp;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let input = &line[..pos];
        Ok(get_completion(input, self.cached(input).as_ref()))
    }
}

impl ConditionalEventHandler for H {
    fn handle(
        &self,
        evt: &Event,
        _n: rustyline::RepeatCount,
        _positive: bool,
        ctx: &rustyline::EventContext,
    ) -> Option<rustyline::Cmd> {
        let input = &ctx.line()[..ctx.pos()];
        get_event_filter(evt, self.cached(&input).as_ref()?)
    }
}

impl Helper for H {}
impl Validator for H {}
impl Highlighter for H {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        match self.cache.peek().as_ref() {
            Some(res) => {
                let r = render_outcome(&res, true);
                Cow::from(r.display)
            }
            None => Cow::from(hint),
        }
    }

    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        match self.cache.peek().as_ref() {
            Some(ParseOutcome::Failure(x)) => Cow::from(colorize_fail(x, line)),
            _ => Cow::from(line),
        }
    }
}

impl Hinter for H {
    type Hint = RustyHint;
    fn hint(&self, line: &str, pos: usize, _ctx: &rustyline::Context<'_>) -> Option<Self::Hint> {
        let input = &line[..pos];
        Some(render_outcome(self.cached(input).as_ref()?, false))
    }
}

fn main() {
    let cfg = rustyline::Config::builder()
        .completion_type(CompletionType::Circular)
        .build();

    let h = H::new();
    let mut rl = rustyline::Editor::<H>::with_config(cfg);
    rl.set_helper(Some(h.clone()));
    rl.bind_sequence(Event::Any, EventHandler::Conditional(Box::new(h.clone())));
    while let Ok(line) = rl.readline(">> ") {
        let parsed = H::parse(&line);
        println!("{:?}", parsed);
    }
}
