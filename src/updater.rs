//! Interactive structure updater
//!
//! Library offers a way to update a structure using [`Updater`] and a macro to derive it.
//!
//! ```ignore
//! #[derive(Updater)]
//! struct Banana {
//!     shape: u32,
//!     color: f64
//! }
//! let mut banana = Banana { shape: 12, color: 3.1415 };
//! let p = updater_for(&banana, "banana");
//! let r = parse_result(&p, "banana . shape 16")?;
//! banana.apply(r);
//! // Banana { shape: 16, color: 3.1415 }
//! ```

use std::borrow::Cow;

use crate::prelude::*;
use crate::Result;

/// Entry point for interactive structure updated
///
/// ```ignore
/// #[derive(Updater)]
/// struct Banana {
///     shape: u32,
///     color: f64
/// }
/// let mut banana = Banana { shape: 12, color: 3.1415 };
/// let p = updater_for(&banana, "banana");
/// let r = parse_result(&p, "banana . shape 16")?;
/// banana.apply(r);
/// // Banana { shape: 16, color: 3.1415 }
/// ```
pub fn updater_for<'a, T>(
    item: &'a T,
    label: &'static str,
) -> impl FnMut(&str) -> Result<T::Updater> + 'a
where
    T: Updater,
{
    move |input| Updater::enter(item, label, input)
}

/// Updater trait for a collection, mostly to be able to update single items inside collections
/// (vector, etc)
pub trait Checker: Updater {
    /// Item type
    type Item;

    /// check functi
    fn element_check<C>(
        &self,
        updater: &Self::Updater,
        check: &C,
    ) -> std::result::Result<(), String>
    where
        C: Fn(&Self::Item, &Self::Item) -> std::result::Result<(), String>;
}

impl<T: Updater<Updater = T> + Parser + std::fmt::Debug> Checker for Vec<T> {
    type Item = T;

    fn element_check<C>(
        &self,
        updater: &Self::Updater,
        check: &C,
    ) -> std::result::Result<(), String>
    where
        C: Fn(&Self::Item, &Self::Item) -> std::result::Result<(), String>,
    {
        match updater {
            UpdateOrInsert::Update(ix, new) => {
                if *ix >= self.len() {
                    Err(format!(
                        "{} is not a valid index for vector of size {}",
                        ix,
                        self.len()
                    ))
                } else {
                    check(&self[*ix], new)
                }
            }
            UpdateOrInsert::Del(_) | UpdateOrInsert::Ins(_, _) => Ok(()),
        }
    }
}

/*

update field with type safe updater coming from the omnomnomicon

sanity check on a field - takes old and new value, returns a vec of errors,
field sanity checkers can access nearby fields from self in scope

sanity check on a whole structure - attach them on top, runs every time there's an
update or at any time with check call - do we care about access to old/new values?
For standalone check those won't be available

bubble up error messages building path to a problematic field

annotation to "no check needed here" - for bools and such

*/

/// Interactive structure updater trait
///
/// This trait creates interactive update menu for a struct
/// See [`updater_for`] for examples
pub trait Updater {
    /// A separate type containing changes to one of the fields in `Self` such that [`Updater::apply`] properties hold.
    ///
    /// For example for a pair of `i32` `type Foo = (i32, i32)` an `Updater` is a `(bool, i32)`,
    /// where `bool` indicates if the `i32` value should replace the value in the first or the second
    /// field.
    ///
    /// Updater is a property of a new data, not field
    type Updater;

    /// Parse an updater for a current item
    ///
    /// For structures containing multiple fields first thing `enter` method shoud do is to
    /// parse `entry` as [`literal`] from the input and ignore its value
    fn enter<'a>(&self, entry: &'static str, input: &'a str) -> Result<'a, Self::Updater>;

    /// Apply changes from [`Self::Updater`] to a current value
    fn apply(&mut self, updater: Self::Updater, errors: &mut Vec<String>);

    /// accumulate error message from this structure
    ///
    /// error messages look like
    /// "message"/field/AdjConfig/TraderConfig
    fn check(&self, acc: &mut Vec<String>);
}

impl<T> Updater for T
where
    T: Parser + std::fmt::Debug,
{
    type Updater = T;
    fn enter<'a>(&self, _: &'static str, input: &'a str) -> Result<'a, Self::Updater> {
        with_hint(
            || Some(Cow::from(format!("cur: {:?}", self))),
            Parser::parse,
        )(input)
    }

    fn apply(&mut self, updater: Self::Updater, _errors: &mut Vec<String>) {
        *self = updater;
    }
    // nothing to do here
    fn check(&self, _errors: &mut Vec<String>) {}
}

impl<T: Parser + Clone + std::fmt::Debug> Updater for Option<T> {
    type Updater = Option<T>;

    fn enter<'a>(&self, _: &'static str, input: &'a str) -> Result<'a, Self::Updater> {
        let enabled = tagged("some", fmap(Some, T::parse));
        let disabled = tag(None, "none");

        with_hint(
            || Some(Cow::from(format!("cur: {:?}", self))),
            or(enabled, disabled),
        )(input)
    }

    fn apply(&mut self, updater: Self::Updater, _errors: &mut Vec<String>) {
        *self = updater;
    }

    fn check(&self, _errors: &mut Vec<String>) {}
}

impl<T: Updater + std::fmt::Debug, const N: usize> Updater for [T; N] {
    type Updater = (usize, T::Updater);

    fn enter<'a>(&self, entry: &'static str, input: &'a str) -> Result<'a, Self::Updater> {
        let label_ix = || Some(Cow::from(format!("arr index, 0..{}", N - 1)));
        let parse_ix = with_hint(label_ix, number::<usize>);
        let (output, _) = literal(entry)(input)?;
        let key_input = output.input;
        let (output, key) = output.bind_space(true, parse_ix)?;
        if key >= N {
            return Terminate::fail(key_input, format!("Index too big, valid range 0..{}", N));
        }
        let (output, val) = output.bind_space(true, |i| self[key].enter(".", i))?;
        Ok((output, (key, val)))
    }

    fn apply(&mut self, updater: Self::Updater, errors: &mut Vec<String>) {
        self[updater.0].apply(updater.1, errors)
    }

    fn check(&self, _errors: &mut Vec<String>) {}
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
/// Updater for for a map like structure
pub enum UpdateOrInsert<K, T> {
    /// Delete item at key `K`
    Del(K),
    /// Insert an item `T` to key `K`
    Ins(K, T),
    /// Update item at key `K` with updater `U`
    Update(K, T),
}

impl<T: Updater<Updater = T> + Parser + std::fmt::Debug> Updater for Vec<T> {
    type Updater = UpdateOrInsert<usize, T>;

    fn enter<'a>(&self, entry: &'static str, input: &'a str) -> Result<'a, Self::Updater> {
        let label_ix = || Some(Cow::from(format!("vec index, 0..{}", self.len())));
        let parse_ix = || with_hint(label_ix, number::<usize>);

        let (output, _) = literal(entry)(input)?;
        let (output, ix) = output.bind_space(true, parse_ix())?;
        let ops = choice((literal("ins"), literal("del"), literal("upd")));
        let (output, op) = output.bind_space(true, ops)?;
        match op {
            "ins" => {
                let (output, val) = output.bind_space(true, label("value to insert", T::parse))?;
                Ok((output, UpdateOrInsert::Ins(ix, val)))
            }
            "del" => Ok((output, UpdateOrInsert::Del(ix))),
            "upd" => {
                let (output, val) = output.bind_space(true, label("value to replace", T::parse))?;
                Ok((output, UpdateOrInsert::Update(ix, val)))
            }
            _ => Terminate::fail(output.input, "unreachable"),
        }
    }

    fn apply(&mut self, updater: Self::Updater, errors: &mut Vec<String>) {
        match updater {
            UpdateOrInsert::Del(ix) => {
                if ix >= self.len() {
                    errors.push(format!("{} is not a valid index in 0..{}", ix, self.len()))
                } else {
                    self.remove(ix);
                }
            }
            UpdateOrInsert::Ins(ix, t) => {
                if ix > self.len() {
                    errors.push(format!("{} is not a valid index in 0..{}", ix, self.len()))
                } else {
                    self.insert(ix, t);
                }
            }
            UpdateOrInsert::Update(ix, u) => {
                if ix > self.len() {
                    errors.push(format!("{} is not a valid index in 0..{}", ix, self.len()))
                } else {
                    self[ix].apply(u, errors)
                }
            }
        }
    }

    fn check(&self, _errors: &mut Vec<String>) {}
}

#[cfg(feature = "enum-map")]
impl<K, V> Updater for enum_map::EnumMap<K, V>
where
    K: enum_map::EnumArray<V> + Copy + std::fmt::Debug,
    V: Updater,
{
    type Updater = (K, <V as Updater>::Updater);

    fn enter<'a>(&self, entry: &'static str, input: &'a str) -> Result<'a, Self::Updater> {
        let table = self.iter().map(|(k, _)| (k, Cow::from(format!("{:?}", k))));
        let (output, _) = literal(entry)(input)?;
        let (output, key) = output.bind_space(true, lookup_key(table, 100))?;
        let (output, val) = output.bind_space(true, |i| self[key].enter(".", i))?;
        Ok((output, (key, val)))
    }

    fn apply(&mut self, (key, updater): Self::Updater, errors: &mut Vec<String>) {
        self[key].apply(updater, errors)
    }

    fn check(&self, _errors: &mut Vec<String>) {}
}

#[test]
fn test_updater() {
    mod foo {
        pub fn ten_percent(orig: &f64, new: &f64) -> std::result::Result<(), String> {
            let diff = (*orig - *new) * 100.0 / (*orig);
            if (-10.0..=10.0).contains(&diff) {
                Ok(())
            } else {
                Err(format!("Change {} -> {} is to large", orig, new))
            }
        }
    }

    fn positive(_: &f64, new: &f64) -> std::result::Result<(), String> {
        if *new > 0.0 {
            Ok(())
        } else {
            Err("New value must be positive".to_owned())
        }
    }
    pub use foo::ten_percent;

    #[derive(Debug, Updater)]
    struct Foo {
        #[om(check(foo::ten_percent))]
        foo: f64,
        #[om(check(ten_percent), check(positive))]
        bar: f64,
    }

    let mut payload = Foo {
        foo: 100.0,
        bar: 100.0,
    };

    let mut errors = Vec::new();

    payload.apply(FooUpdater::Foo(95.0), &mut errors);
    assert!(errors.is_empty());
    payload.apply(FooUpdater::Foo(65.0), &mut errors);
    assert!(!errors.is_empty());
    errors.clear();

    payload.bar = -100.0;
    payload.apply(FooUpdater::Bar(-99.0), &mut errors);
    assert!(!errors.is_empty());
}

// TODO HashMap
