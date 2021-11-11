use crate::Result;

/// Helper trait for [`choice`] combinator.
pub trait Choice<R> {
    /// see [`choice`]
    fn choice<'a>(&mut self, input: &'a str) -> Result<'a, R>;
}

/// Apply parsers one by one until one succeeds
///
/// Function takes a tuple of parsers and tries them all one by one until one succeeds.
/// If all of them fail [`choice`] generates an appropriate completion information.
///
/// # Examples
/// ```rust
/// use omnomnomicon::prelude::*;
/// let p1 = literal("hello");
/// let p2 = literal("hey");
/// let p3 = literal("hi");
/// let p = choice((p1, p2, p3));
///
/// let r = parse_result(p, "hi")?;
/// // "hi"
/// # assert_eq!(r, "hi");
/// # Ok::<(), String>(())
/// ```
pub fn choice<P: Choice<R>, R>(mut parsers: P) -> impl FnMut(&str) -> Result<R> {
    move |input| parsers.choice(input)
}

macro_rules! derive_choice {
    (@try_parser $step:tt, $parsers:ident, $input:ident, $term:ident,) => {};
    (@try_parser $step:tt, $parsers:ident, $input:ident, $term:ident, $_next:ident $($result:ident)*) => {
        $term = match $parsers.$step($input) {
            Err(err) => $term + err,
            ok => return ok
        };
        succ!($step, derive_choice!(@try_parser, $parsers, $input, $term, $($result)*));
    };

    (@mk_impl $fparser:ident $($parser:ident)+) => {
        impl<$fparser, $($parser),+, R> Choice<R> for ($fparser, $($parser),+)
        where
            $fparser: FnMut(&str) -> Result<R>,
            $( $parser: FnMut(&str) -> Result<R>),+
        {
            fn choice<'a>(&mut self, input: &'a str) -> Result<'a, R> {
                let mut term = match self.0(input) {
                    Err(err) => err,
                    ok => return ok,
                };

                derive_choice!(@try_parser 1, self, input, term, $($parser)+);
                return Err(term)
            }
        }
    };


    ($fparser:ident $($parser:ident)+) => {
        derive_choice!(@mk_impl $fparser $($parser)+);
        derive_choice!($($parser)+);
    };
    ($fparser:ident) => {};
}

derive_choice!(P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P11 P12 P13 P14 P15 P16 P17 P18 P19 P20 P21 P22 P23 P24 P25 P26);
