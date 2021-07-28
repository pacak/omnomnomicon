//! Instances and parsing functions for `chrono`

use crate::{decorators::label, parsers::literal, *};

/// `Parser` for [`Duration`][::chrono::Duration]
///
/// Accepts a number followed by a suffix:
/// - `ns` - nanoseconds
/// - `us` - microseconds
/// - `ms` - milliseconds
/// - `s` - seconsd
/// - `m` - minutes
/// - `h` - hours
/// - `d` - days
/// - `w` - weeks
///
/// Number _can_ contain fractional part and will not panic
impl Parser for ::chrono::Duration {
    fn parse(input: &str) -> Result<Self> {
        use self::prelude::*;
        #[derive(Clone)]
        enum Suffix {
            Nano,
            Micro,
            Milli,
            Secs,
            Mins,
            Hours,
            Days,
            Weeks,
        }
        let s1 = tag(Suffix::Nano, "ns");
        let s2 = tag(Suffix::Micro, "us");
        let s3 = tag(Suffix::Milli, "ms");
        let s4 = tag(Suffix::Secs, "s");
        let s5 = tag(Suffix::Mins, "m");
        let s6 = tag(Suffix::Hours, "h");
        let s7 = tag(Suffix::Days, "d");
        let s8 = tag(Suffix::Weeks, "w");
        let ss = choice((s1, s2, s3, s4, s5, s6, s7, s8));
        let (output, mut value) = <f64 as Parser>::parse(input)?;
        let (output, suffix) = output.bind(ss)?;
        match suffix {
            Suffix::Nano => value /= 1e9,
            Suffix::Micro => value /= 1e6,
            Suffix::Milli => value /= 1e3,
            Suffix::Secs => {}
            Suffix::Mins => value *= 60.0,
            Suffix::Hours => value *= 3600.0,
            Suffix::Days => value *= 86400.0,
            Suffix::Weeks => value *= 604800.0,
        }
        // go home clippy, you're drunk
        #[allow(clippy::manual_range_contains)]
        if value < 0.0 || value > 1e18 {
            Terminate::fail(input, "not a valid duration")
        } else {
            let nanos = (value * 1e9) as i64;
            Ok((output, ::chrono::Duration::nanoseconds(nanos)))
        }
    }
}

#[test]
fn test_duration() {
    use crate::prelude::parse_result;
    use ::chrono::Duration;
    assert_eq!(
        parse_result(Duration::parse, "1.5s").unwrap(),
        Duration::milliseconds(1500)
    );
    assert_eq!(
        parse_result(Duration::parse, "1.5w").unwrap(),
        Duration::hours(252)
    );
}

/// `Parser` for [`NaiveTime`][::chrono::naive::NaiveTime]
///
/// Accepts time in 24 hours format, valid strings are:
/// - `HH:MM`
/// - `HH:MM:SS`
/// - `HH:MM:SS.s+`
impl Parser for ::chrono::naive::NaiveTime {
    fn parse(input: &str) -> Result<Self> {
        let (o, hour) = label("HH", two_digits(0..24))(input)?;
        let (o, _) = o.bind(literal(":"))?;
        let (o, minute) = o.bind(label("MM", two_digits(0..60)))?;

        let h_m_input = o.input;
        let (o, _) = match o.bind(literal(":")) {
            Ok(ok) => ok,
            Err(err) => {
                let output = match err {
                    Terminate::Eof(state) => Output::new(h_m_input, state),
                    Terminate::Failure(_) => Output::disabled(h_m_input),
                };
                let val = Self::from_hms(hour, minute, 0);
                return Ok((output, val));
            }
        };
        let (o, second) = o.bind(label("SS", two_digits(0..60)))?;
        let h_m_s_input = o.input;
        let (o, _) = match o.bind(literal(".")) {
            Ok(ok) => ok,
            Err(err) => {
                let output = match err {
                    Terminate::Eof(state) => Output::new(h_m_s_input, state),
                    Terminate::Failure(_) => Output::disabled(h_m_input),
                };
                let val = Self::from_hms(hour, minute, second);
                return Ok((output, val));
            }
        };
        let nano_input = o.input;
        let (o, frac) = o.bind(label("q", u32::parse))?;
        let digits = nano_input.len() - o.input.len();

        if digits > 9 {
            return Terminate::fail(nano_input, "too many digits");
        }
        let nano = frac * 10u32.pow(9 - digits as u32);
        let value = ::chrono::NaiveTime::from_hms_nano(hour, minute, second, nano);
        Ok((o, value))
    }
}

fn two_digits(r: std::ops::Range<u32>) -> impl Fn(&str) -> Result<u32> {
    move |input| {
        let (output, val) = u32::parse(input)?;
        if input.len() - output.input.len() == 2 && r.contains(&val) {
            Ok((output, val))
        } else if input.len() < 2 {
            Terminate::eof()
        } else {
            Terminate::fail(input, "Expect 2 digits in a valid range")
        }
    }
}

#[test]
fn test_naivetime() {
    use crate::prelude::parse_result;
    use ::chrono::naive::NaiveTime;
    assert_eq!(
        parse_result(NaiveTime::parse, "00:01").unwrap(),
        NaiveTime::from_hms(0, 1, 0)
    );
    assert_eq!(
        parse_result(NaiveTime::parse, "01:02:03").unwrap(),
        NaiveTime::from_hms(1, 2, 3)
    );
    assert_eq!(
        parse_result(NaiveTime::parse, "01:02:03.1").unwrap(),
        NaiveTime::from_hms_milli(1, 2, 3, 100)
    );
    assert_eq!(
        parse_result(NaiveTime::parse, "01:02:03.123").unwrap(),
        NaiveTime::from_hms_milli(1, 2, 3, 123)
    );
    assert_eq!(
        parse_result(NaiveTime::parse, "01:02:03.123456").unwrap(),
        NaiveTime::from_hms_micro(1, 2, 3, 123456)
    );
    assert_eq!(
        parse_result(NaiveTime::parse, "01:02:03.123456789").unwrap(),
        NaiveTime::from_hms_nano(1, 2, 3, 123456789)
    );
}

/// `Parser` for [`NaiveDate`][::chrono::naive::NaiveDate]
///
/// Accepts date in `YYYY-MM-DD` format, supports a wide range of dates.
/// Month and Day are two digits.
impl Parser for ::chrono::naive::NaiveDate {
    fn parse(input: &str) -> Result<Self> {
        let (o, year) = label("year", i32::parse)(input)?;
        let (o, _) = o.bind(literal("-"))?;
        let (o, month) = o.bind(label("month", two_digits(1..13)))?;
        let (o, _) = o.bind(literal("-"))?;
        let (o, day) = o.bind(label("day", two_digits(1..32)))?;
        match ::chrono::naive::NaiveDate::from_ymd_opt(year, month, day) {
            Some(val) => Ok((o, val)),
            None => Terminate::fail(input, "Not a valid date"),
        }
    }
}

#[test]
fn test_naivedate() {
    use crate::prelude::parse_result;
    use ::chrono::naive::NaiveDate;
    assert_eq!(
        parse_result(NaiveDate::parse, "1-01-01").unwrap(),
        NaiveDate::from_ymd(1, 1, 1)
    );
    assert_eq!(
        parse_result(NaiveDate::parse, "2020-05-20").unwrap(),
        NaiveDate::from_ymd(2020, 5, 20)
    );
}

/// `Parser` for [`NaiveDateTime`][::chrono::naive::NaiveDateTime]
///
/// Accepts date in `YYYY-MM-DD HH:MM:SS.q` format, supports a wide range of dates.
/// Month and Day are two digits.
impl Parser for ::chrono::naive::NaiveDateTime {
    fn parse(input: &str) -> Result<Self> {
        use crate::combinators::{fmap, words};
        use ::chrono::naive::{NaiveDate, NaiveTime};

        fmap(
            |(d, t)| d.and_time(t),
            words((NaiveDate::parse, NaiveTime::parse)),
        )(input)
    }
}

#[test]
fn test_naivedatetime() {
    use crate::prelude::parse_result;
    use ::chrono::naive::{NaiveDate, NaiveDateTime, NaiveTime};
    assert_eq!(
        parse_result(NaiveDateTime::parse, "2020-05-20 00:11:12.123").unwrap(),
        NaiveDate::from_ymd(2020, 5, 20).and_time(NaiveTime::from_hms_milli(0, 11, 12, 123))
    );
}
