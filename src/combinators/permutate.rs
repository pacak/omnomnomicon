use crate::state::State;
use crate::{Output, Result, Terminate};

/// Helper trait for [`perm`] combinator.
pub trait Perm<R> {
    /// see [`perm`]
    fn perm<'a>(&mut self, input: &'a str) -> Result<'a, R>;
}

/// Apply parsers in arbitrary order until all succeed
///
/// Apply a group of parsers in arbitrary order order until all of them succeed or none of the
/// unused ones can't parse current leftovers.
///
/// # Performance
/// If subparser succeeds without consuming any input `perm` will try to use the same subarser again until it succeeds consuming something.
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p1 = literal("a");
/// let p2 = option(literal("b"));
/// let p3 = literal("c");
/// let mut p = perm((p1, p2, p3));
///
/// let r = parse_result(&mut p, "abc")?;
/// // ("a", Some("b"), "c")
/// # assert_eq!(r, ("a", Some("b"), "c"));
///
/// let r = parse_result(&mut p, "bca")?;
/// // ("a", Some("b"), "c")
/// # assert_eq!(r, ("a", Some("b"), "c"));
///
/// let r = parse_result(&mut p, "ca")?;
/// // ("a", None, "c")
/// # assert_eq!(r, ("a", None, "c"));
///
/// let r = parse_result(&mut p, "cab")?;
/// // ("a", Some("b"), "c")
/// # assert_eq!(r, ("a", Some("b"), "c"));
///
/// # Ok::<(), String>(())
/// ```
pub fn perm<P: Perm<R>, R>(mut parsers: P) -> impl FnMut(&str) -> Result<R> {
    move |input| parsers.perm(input)
}

fn pass<'a, P, R>(result: &mut Option<(bool, R)>, mut parser: P, st: &mut St<'a>)
where
    P: FnMut(&'a str) -> Result<'a, R>,
{
    if let Some((true, _)) = result {
        return;
    }
    match parser(st.input) {
        Ok((output, r)) => {
            if st.input.len() != output.input.len() {
                st.consumed_this_iteration = true;
                if output.state.is_enabled() {
                    st.perm_state.push(output.state)
                } else {
                    st.perm_state.clear()
                }
                st.input = output.input;
                *result = Some((true, r));
            } else {
                if output.state.is_enabled() {
                    st.loop_state.push(output.state);
                }
                *result = Some((false, r));
            }
        }
        Err(Terminate::Eof(state)) => {
            st.loop_state.push(state);
        }
        Err(_term) => {}
    }
}

#[derive(Debug)]
struct St<'a> {
    /// results from consuming successes
    perm_state: Vec<State>,
    /// results from nonconsuming successes or failures
    loop_state: Vec<State>,
    /// there's been a _consuming_ improvement this iteration
    consumed_this_iteration: bool,
    input: &'a str,
}

impl<'a> St<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            perm_state: Vec::new(),
            loop_state: Vec::new(),
            consumed_this_iteration: false,
            input,
        }
    }

    fn loop_start(&mut self) {
        self.loop_state.clear();
        self.consumed_this_iteration = false;
    }
}

macro_rules! derive_perm {
    (@collect_term $step:tt, $r:ident, $term:ident,) => {};
    (@collect_term $step:tt, $r:ident, $term:ident, $_next:ident $($result:ident)*) => {
        $term = $term + $r.$step.get_term();
        succ!($step, derive_perm!(@collect_term, $r, $term, $($result)*));
    };

    (@parse_step $step:tt, $parsers:ident, $st:ident, $r:ident, ) => {};
    (@parse_step $step:tt, $parsers:ident, $st:ident, $r:ident, $_next:ident $($result:ident)*) => {
        pass(&mut $r . $step, &mut $parsers . $step, &mut $st);
        succ!($step, derive_perm!(@parse_step, $parsers, $st, $r, $($result)*));
    };

    (@mk_impl $($parser:ident $result:ident),+) => {
        impl<$($parser),+, $($result),+> Perm<($($result),+)> for ($($parser),+)
        where
            $( $parser: FnMut(&str) -> Result<$result>),+
        {
            fn perm<'a>(&mut self, input: &'a str) -> Result<'a, ($($result),+)> {
                let mut r = ( $( <Option<(bool, $result)>>::None),+);
                let mut st = St::new(input);
                loop {
                    st.loop_start();
                    derive_perm!(@parse_step 0, self, st, r, $($result)+);
                    if !st.consumed_this_iteration {
                        break
                    }
                }

                let state= st
                    .perm_state
                    .into_iter()
                    .chain(st.loop_state.into_iter())
                    .reduce(|a, b| a + b).unwrap_or_else(State::disabled);

                #[allow(non_snake_case)]
                match r {
                    ($( Some( $result ) ),+) => {

                        Ok((Output::new(st.input, state), ($( $result . 1),+)))
                    }

                    _ => if state.is_enabled() {
                        Err(Terminate::Eof(state))
                    } else {
                        Err(Terminate::default())
                    },

                }

            }
        }
    };


    ($fparser:ident $fresult:ident, $($parser:ident $result:ident),+) => {
        derive_perm!($($parser $result),+);
        derive_perm!(@mk_impl $fparser $fresult, $($parser $result),+);
    };

    ($parser:ident $result:ident) => {};
}

derive_perm!(P1 R1, P2 R2, P3 R3, P4 R4, P5 R5, P6 R6, P7 R7, P8 R8, P9 R9, P10 R10);

#[test]
fn test_words() {
    use crate::prelude::*;
    let p1 = literal("a");
    let p2 = option(literal("b"));
    let p3 = literal("c");
    let mut p = perm((p1, p2, p3));
    assert_eq!(parse_result(&mut p, "ca").unwrap(), ("a", None, "c"));
    assert_eq!(parse_result(&mut p, "abc").unwrap(), ("a", Some("b"), "c"));
    assert_eq!(parse_result(&mut p, "bca").unwrap(), ("a", Some("b"), "c"));
    assert_eq!(parse_result(&mut p, "cab").unwrap(), ("a", Some("b"), "c"));
}
