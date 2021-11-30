use crate::Result;

/// Helper trait for [`tuple`][tuple()] combinator
pub trait Tuple<R> {
    /// see [`tuple`][tuple()]
    fn tuple<'a>(&mut self, input: &'a str) -> Result<'a, R>;
}

/// Apply parsers sequentially
///
/// Applies each parser sequentially collecting results into a tuple, if any parser fails - whole
/// combination fails.
/// Short one line description explaining what function does
///
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p1 = literal("hello");
/// let p2 = number::<u16>;
/// let mut p = tuple((p1, p2));
/// let r = parse_result(p, "hello12")?;
/// // (hello, 12)
/// # assert_eq!(r, ("hello", 12));
/// # Ok::<(), String>(())
/// ```
pub fn tuple<P, R>(mut parsers: P) -> impl FnMut(&str) -> Result<R>
where
    P: Tuple<R>,
{
    move |input| parsers.tuple(input)
}

macro_rules! derive_tuple {
    (@parse_step 0, $parsers:ident, $output:ident, $next:ident $($result:ident)*) => {
        let ($output, $next) = $parsers.0($output)?;
        derive_tuple!(@parse_step 1, $parsers, $output, $($result)+);
    };

    (@parse_step $step:tt, $parsers:ident, $output:ident, ) => {};
    (@parse_step $step:tt, $parsers:ident, $output:ident, $next:ident $($result:ident)*) => {
        let ($output, $next) = $output.bind(&mut $parsers.$step)?;
        succ!($step, derive_tuple!(@parse_step, $parsers, $output, $($result)*))
    };

    (@mk_impl $($parser:ident $result:ident),+) => {
        impl<$($parser, $result),+> Tuple<($($result),+)> for ($($parser),+)
        where
            $($parser: for<'s>FnMut(&'s str) -> Result<'s, $result>),+
        {
                        #[allow(non_snake_case)]
            fn tuple<'a>(&mut self, input: &'a str) -> Result<'a, ($($result),+)> {
                derive_tuple!(@parse_step 0, self, input, $($result)+);
                Ok((input, ($($result),+)))
            }
        }
    };

    ($fparser:ident $fresult:ident, $($parser:ident $result:ident),+) => {
        derive_tuple!($($parser $result),+);
        derive_tuple!(@mk_impl $fparser $fresult, $($parser $result),+);
    };
    ($fparser:ident $fresult:ident) => {};
}

derive_tuple!(P1 R1, P2 R2, P3 R3, P4 R4, P5 R5, P6 R6, P7 R7, P8 R8, P9 R9, P10 R10, P11 R11, P12 R12, P13 R13);

/// Helper trait for [`words`] combinator
pub trait Words<R> {
    /// See [`words`]
    fn words<'a>(&mut self, input: &'a str) -> Result<'a, R>;
}

/// Parse several space separated items
///
/// Applies each parser sequentially handling it as a separate word, basically the same as [`tuple`][tuple()]
/// but with extra spaces between parsers.
/// # Examples
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = words((number::<u32>, number::<u16>));
/// let r = parse_result(p, "123 456")?;
/// // (123, 456)
/// # assert_eq!(r, (123, 456));
/// # Ok::<(), String>(())
/// ```
///
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = words((number::<u32>, option(number::<u32>)));
/// let r = parse_result(p, "123")?;
/// // (123, None)
/// # assert_eq!(r, (123, None));
/// # Ok::<(), String>(())
/// ```
///
/// Note, there's still no backtracking once a parser succeeds: here 123 is captured by the
/// first number which leaves no input to the second one
/// ```rust
/// # use omnomnomicon::prelude::*;
/// let p = words((option(number::<u16>), number::<u16>));
/// let r = parse_result(p, "123").is_err();
/// // fails to parse
/// # assert_eq!(r, true);
/// # Ok::<(), String>(())
/// ```
pub fn words<P, R>(mut parsers: P) -> impl for<'s> FnMut(&'s str) -> Result<'s, R>
where
    P: Words<R>,
{
    move |input| parsers.words(input)
}

macro_rules! derive_words {
    (@parse_step 0, $parsers:ident, $output:ident, $next:ident $($result:ident)*) => {
        let input_len = $output.len();
        let ($output, $next) = $parsers.0($output)?;
        let mut consumed = $output.input.len() < input_len;
        derive_words!(@parse_step 1, $parsers, $output, consumed, $($result)+);
    };

    (@parse_step $step:tt, $parsers:ident, $output:ident, $consumed:ident, ) => {let _ = $consumed; };
    (@parse_step $step:tt, $parsers:ident, $output:ident, $consumed:ident, $next:ident $($result:ident)*) => {
        let input_len = $output.input.len();
        let ($output, $next) = $output.bind_space( $consumed, &mut $parsers.$step)?;
        $consumed |= input_len > $output.input.len();
        succ!($step, derive_words!(@parse_step, $parsers, $output, $consumed, $($result)*))
    };

    (@mk_impl $($parser:ident $result:ident),+) => {
        impl<$($parser, $result),+> Words<($($result),+)> for ($($parser),+)
        where
            $($parser: for <'s> FnMut(&'s str) -> Result<'s, $result>),+
        {
            #[allow(non_snake_case)]
            fn words<'a>(&mut self, input: &'a str) -> Result<'a, ($($result),+)> {
                derive_words!(@parse_step 0, self, input, $($result)+);
                Ok((input, ($($result),+)))
            }
        }
    };

    ($fparser:ident $fresult:ident, $($parser:ident $result:ident),+) => {
        derive_words!($($parser $result),+);
        derive_words!(@mk_impl $fparser $fresult, $($parser $result),+);
    };
    ($fparser:ident $fresult:ident) => {};
}
derive_words!(P1 R1, P2 R2, P3 R3, P4 R4, P5 R5, P6 R6, P7 R7, P8 R8, P9 R9, P10 R10, P11 R11, P12 R12, P13 R13);

#[cfg(test)]
mod test {
    use crate::prelude::*;

    #[test]
    fn test_words() {
        let p1 = label("p1", literal("p1"));
        let p2 = label("p2", literal("p2"));
        let p3 = label("p3", literal("p3"));

        let mut p = words((p1, option(p2), p3));
        let r = parse_hints(&mut p, "p1").unwrap().replacements();
        assert_eq!(r, &[" "]);
    }

    #[test]
    fn test_snd_word() {
        let p = snd_word(literal("mask"), number::<u32>);
        let mut p = fmap(|_| (), p);
        let r = parse_hints(&mut p, "").unwrap().replacements();
        assert_eq!(r, &["mask"]);
        let r = parse_hints(&mut p, "ma").unwrap().replacements();
        assert_eq!(r, &["mask"]);
    }
}
