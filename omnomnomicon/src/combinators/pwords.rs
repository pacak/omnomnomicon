use crate::*;

/// Helper trait for [`pwords`] combinator.
pub trait PermWords<R> {
    /// see [`pwords`]
    fn pwords<'a>(&mut self, input: &'a str) -> Result<'a, R>;
}

/// Apply parsers in arbitrary order until all succeed with extra spaces between them
///
/// Apply a group of parsers in arbitrary order order until all of them succeed or none of the
/// unused ones can't parse current leftovers. Subparsers can fallback to default values
/// ([`option`][crate::combinators::option])
///
/// # Performance
/// If parser succeeds without consuming any input `perm` will attempt to use it in subsequent passes.
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p1 = literal("a");
/// let p2 = option(literal("b"));
/// let p3 = literal("c");
/// let mut p = pwords((p1, p2, p3));
///
/// let r = parse_result(&mut p, "a b c")?;
/// // ("a", Some("b"), "c")
/// # assert_eq!(r, ("a", Some("b"), "c"));
///
/// let r = parse_result(&mut p, "b c a")?;
/// // ("a", Some("b"), "c")
/// # assert_eq!(r, ("a", Some("b"), "c"));
///
/// let r = parse_result(&mut p, "c a")?;
/// // ("a", None, "c")
/// # assert_eq!(r, ("a", None, "c"));
///
/// let r = parse_result(&mut p, "c a b")?;
/// // ("a", Some("b"), "c")
/// # assert_eq!(r, ("a", Some("b"), "c"));
///
/// # Ok::<(), String>(())
/// ```
pub fn pwords<P: PermWords<R>, R>(mut parsers: P) -> impl FnMut(&str) -> Result<R> {
    move |input| parsers.pwords(input)
}

// perm words
//
// 1. apply parsers in any order
// 2. consuming parsers must be separated by a space
// 3. non consuming parsers are checked after each consumption in case it becomes consuming
// 4. once all the parsers satisfied we collect the results

#[derive(Debug)]
struct St<'a> {
    /// results from consuming successes
    perm_state: Vec<State>,
    /// results from nonconsuming successes or failures
    loop_state: Vec<State>,
    /// there's been a _consuming_ improvement this iteration
    consumed_this_iteration: bool,
    /// all the results are available or there's a problem
    terminate: bool,
    start_of_sequence: bool,
    input: Input<'a>,
}

impl<'a> St<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            perm_state: Vec::new(),
            loop_state: Vec::new(),
            consumed_this_iteration: false,
            start_of_sequence: true,
            terminate: false,
            input: Input {
                raw: input,
                trimmed: input,
            },
        }
    }

    fn loop_start(&mut self) {
        self.consumed_this_iteration = false;
        self.loop_state.clear();
    }
}

macro_rules! derive_pwords {

    (@collect_term $step:tt, $r:ident, $term:ident,) => {};
    (@collect_term $step:tt, $r:ident, $term:ident, $_next:ident $($result:ident)*) => {
        match &$r.$step {
            PermResult::Term(t) => println!("+ {:?}", t),
            _ => {},
        }
        $term = $term + $r.$step.get_term();
        succ!($step, derive_pwords!(@collect_term, $r, $term, $($result)*));
    };

    (@parse_step $step:tt, $parsers:ident, $st:ident, $r:ident, ) => {};
    (@parse_step $step:tt, $parsers:ident, $st:ident, $r:ident, $_next:ident $($result:ident)*) => {
        pass(&mut $r . $step, &mut $parsers . $step, &mut $st);
        succ!($step, derive_pwords!(@parse_step, $parsers, $st, $r, $($result)*));
    };


    (@mk_impl $($parser:ident $result:ident),+) => {
        impl<$($parser),+, $($result),+> PermWords<($($result),+)> for ($($parser),+)
        where
            $( $parser: FnMut(&str) -> Result<$result>),+
        {
            fn pwords<'a>(&mut self, input: &'a str) -> Result<'a, ($($result),+)> {
                let mut r = ( $( <Option<(bool, $result)>>::None ),+);
                let mut st = St::new(input);

                loop {
                    st.loop_start();
                    derive_pwords!(@parse_step 0, self, st, r, $($result)+);
                    if !st.consumed_this_iteration {
                        break
                    }
                }

                let mut state = st
                    .perm_state
                    .into_iter()
                    .chain(st.loop_state.into_iter())
                    .reduce(|a, b| a + b).unwrap_or_else(State::disabled);

                #[allow(non_snake_case)]
                match r {
                    ($( Some( $result ) ),+) => {

                        // there are parsers that still can consume something and we don't (yet)
                        // have a space
                        if !( $( $result . 0) &&+) && !st.input.has_space() {
                            if st.input.raw.is_empty() {
                                state.push(Comp::simple(" ", st.input.raw.len()));
                            }
                        };
                        let output = Output::new(st.input.raw, state);
                        Ok((output, ($( $result . 1),+)))
                    }

                    _ => if state.is_enabled() {
                        Err(Terminate::Eof(state))
                    } else if st.input.raw.is_empty() {
                        Err(Terminate::Eof(State::from(Comp::simple( " ", st.input.raw.len()))))
                    } else {
                        Err(Terminate::default())
                    },

                }

            }
        }
    };


    ($fparser:ident $fresult:ident, $($parser:ident $result:ident),+) => {
        derive_pwords!($($parser $result),+);
        derive_pwords!(@mk_impl $fparser $fresult, $($parser $result),+);
    };

    ($parser:ident $result:ident) => {};

}

#[derive(Copy, Clone, Debug)]
struct Input<'a> {
    raw: &'a str,
    trimmed: &'a str,
}

impl Input<'_> {
    fn has_space(&self) -> bool {
        self.raw.len() > self.trimmed.len()
    }
}

fn pass<'a, P, R>(result: &mut Option<(bool, R)>, mut parser: P, st: &mut St<'a>)
where
    P: FnMut(&'a str) -> Result<'a, R>,
{
    if let Some((true, _)) = result {
        return;
    }
    match parser(st.input.trimmed) {
        Ok((output, r)) => {
            if st.input.trimmed.len() > output.input.len() {
                st.start_of_sequence = false;
                // We managed to parse and consume something
                // this is only valid when we are not terminating the parser
                if st.terminate {
                    return;
                }

                *result = Some((true, r));

                // disabled breaks the info chain
                if output.state.is_enabled() {
                    st.perm_state.push(output.state)
                } else {
                    st.perm_state.clear()
                }

                st.input.raw = output.input;
                // succeeding means we should do one more iteration later
                st.consumed_this_iteration = true;

                st.input.trimmed = output.input.trim_start();

                st.terminate = st.input.raw.len() == st.input.trimmed.len();
            } else {
                // parse succeeds without consuming anything
                // this means parser falls back to default value
                // We'll have to try again later
                *result = Some((false, r));

                // suggestion for non consuming parser only makes sense
                // when we can actually enter it - either at the beginning of the
                // parser or after a space
                if output.state.is_enabled() && (st.input.has_space() || st.start_of_sequence) {
                    st.loop_state.push(output.state);
                }
            }
        }

        Err(Terminate::Eof(state)) => {
            if st.input.has_space() || st.start_of_sequence {
                st.loop_state.push(state);
            }
        }
        Err(_term) => {}
    }
}

#[test]
fn test_pwords_hints() {
    use crate::prelude::*;
    let p1 = label("p1", literal("a"));
    let p2 = label("p2", option(literal("b")));
    let p3 = label("p3", option(literal("c")));
    let mut p = pwords((p1, p2, p3));
    //let p = pwords2(p1, p2, p3);

    assert_eq!(
        parse_result(&mut p, "b c a").unwrap(),
        ("a", Some("b"), Some("c"))
    );
    assert_eq!(parse_result(&mut p, "c a").unwrap(), ("a", None, Some("c")));
    assert_eq!(parse_result(&mut p, "a").unwrap(), ("a", None, None));
    assert_eq!(
        parse_result(&mut p, "b a c").unwrap(),
        ("a", Some("b"), Some("c"))
    );
    assert_eq!(
        parse_result(&mut p, "b a b").unwrap(),
        ("a", Some("b"), None)
    );

    let mut labels = |s| {
        let mut v = parse_hints(&mut p, s).unwrap().labels.to_vec();
        v.sort();
        v
    };

    assert_eq!(labels(""), ["p1", "p2", "p3"]);

    assert_eq!(labels("a").len(), 0);
    assert_eq!(labels("b").len(), 0);
    assert_eq!(labels("c").len(), 0);

    assert_eq!(labels("a "), ["p2", "p3"]);
    assert_eq!(labels("b "), ["p1", "p3"]);
    assert_eq!(labels("c "), ["p1", "p2"]);

    assert_eq!(labels("a b").len(), 0);
    assert_eq!(labels("b a").len(), 0);
    assert_eq!(labels("a c").len(), 0);
    assert_eq!(labels("b c").len(), 0);

    assert_eq!(labels("a b "), ["p3"]);
    assert_eq!(labels("a c "), ["p2"]);
    assert_eq!(labels("b c "), ["p1"]);

    let mut comps = |s| {
        let mut v = parse_hints(&mut p, s)
            .unwrap()
            .comps
            .into_iter()
            .map(|c| c.replacement)
            .collect::<Vec<_>>();
        v.sort();
        v
    };

    assert_eq!(comps(""), ["a", "b", "c"]);

    assert_eq!(comps("a"), [" "]);
    assert_eq!(comps("b"), [" "]);
    assert_eq!(comps("c"), [" "]);

    assert_eq!(comps("a "), ["b", "c"]);
    assert_eq!(comps("b "), ["a", "c"]);
    assert_eq!(comps("c "), ["a", "b"]);

    assert_eq!(parse_hints(&mut p, "a b c").is_err(), true);
}

derive_pwords!(P1 R1, P2 R2, P3 R3, P4 R4, P5 R5, P6 R6, P7 R7, P8 R8, P9 R9, P10 R10);
