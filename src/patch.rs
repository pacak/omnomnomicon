//! Typesafe and value safe way to update a structure
//!
//! # Motivation and goals
//!
//! The main idea of the [`Patch`] trait is to is to provide a type safe way to update values
//! within a nested structures with two extra properties:
//! - check that the value by itself is "sane" - or get a path/error message to a value that fails
//!   a check
//! - during every update it is possible to run a second kind of checks that gets original value +
//!   patch and declares update to be "sane" or containing the error.
//!
//! Both checks combined allow to maintain invariants such as:
//! - f64 fields within a structure are non-NaN/Infinity
//! - numeric values can be incremented by at most 50%
//!
//! See [`Patch`] for more information

#[cfg(doc)]
use crate::literal;
use crate::Parser;
use crate::*;
use std::borrow::Cow;
//use crate::{with_hint, Parser, Result};

/// `Patch` is best thought of associating a value `T` with type `U`, such that `T.apply(U)` gives `T`
/// with a new value.
///
/// For that `Patch` comes with an assocated type [`Update`](Patch::Update) and member [`check`](Patch::check)
///
/// Two remaining function help to parse the update from a string and to run the checks and collect
/// the results.
///
/// # Deriving `Patch`
/// You can derive `Patch` with a derive macro, it supports attributes prefixed with `#[om(xxx)]`
/// ```no_run
/// # use omnomnomicon::*;
/// fn item_checker(item: &Item) -> std::result::Result<(), String> {
///     todo!()
/// }
///
/// fn small_increment(from: &u32, to: &u32) -> std::result::Result<(), String> {
///     todo!()
/// }
///
/// #[derive(Debug, Clone, Patch)]
/// #[om(check(item_checker))] // (1) <- will check the whole structure
/// struct Item {
///     #[om(skip)] // (2)
///     pub field: u64,
///     #[om(dcheck(small_increment))] // (3) <- will check just this field
///     pub value: u32,
/// }
/// ```
///
/// By default `Patch` wants you to specify checks on all the fields, with `no_check` (or `skip`) annotation in
/// (2) or (3) you can derive `Patch` with no checks on a field, alternatively you can place it top
/// level - (1) to disable this requirement for all the fields.
///
/// Following attribute names are available:
/// `skip` - ignore this field when generating patcher, can only be used on fields directly
/// `no_check` - allow to skip check requirements on a field/structure.
/// `check`, `dcheck` - absolute and incremental checks - on a field or structure
/// `dcheck` gets a reference to current value as a first argument and a reference to updater as
/// the second one. When working with primitive fields those match
///
/// # Deriving `Patch`
/// ```no_run
/// # use omnomnomicon::*;
/// fn whole(item: &Foo) -> std::result::Result<(), String> {
///     todo!()
/// }
///
/// fn field_check(item: &u64) -> std::result::Result<(), String> {
///     todo!()
/// }
///
/// fn diff_check(item: &u32, update: &u32) -> std::result::Result<(), String> {
///     todo!()
/// }
///
///
/// #[derive(Debug, Clone, Patch)]
/// #[om(check(whole))] // <- check will get whole Foo structure as input
/// struct Foo {
///     #[om(check(field_check))] // <- check will get only field1 as input
///     pub field1: u64,
///     #[om(check(field_check))]
///     pub field2: u64,
///     #[om(dcheck(diff_check))]
///     pub field3: u32,
///     #[om(no_check)]
///     pub bar: Bar,
/// }
///
/// #[derive(Debug, Clone, Patch)]
/// #[om(no_check)]
/// pub struct Bar {
///     pub field: f64,
/// }
/// ```
pub trait Patch {
    /// Update type
    ///
    /// - For primitive types it might be a value itself.
    /// - For structs or containers - it's usually a way to focus on a field
    ///   + updater for that field.
    /// - for containers that don't have a fixed size such as `Vec` it can be a enum of actions:
    ///   `Insert(Key, Val)`, `Delete(Key)`, `Change(Self::Update)`. You'll have to derive it on
    ///   your own though, `Patch` derive macro can't deal with that type of collections.
    type Update;

    /// Entry point for the parser
    ///
    /// When making an entry point for you manual implementation you should parse
    /// `entry` as a [`literal`] then parse the values themselves
    fn enter<'a>(&self, entry: &'static str, input: &'a str) -> crate::Result<'a, Self::Update>;

    /// Apply the update, collect the errors into the vector.
    ///
    /// Note - when used diretly from the trait - value will be updated even with the errors
    /// You should clone before applying the change and decide after running both `apply` and
    /// `check`. `apply` only runs differential checksm not the sanity check
    fn apply(&mut self, update: Self::Update, errors: &mut Vec<String>);

    /// Run validation check, collect the results
    ///
    /// Does nothing for most of the instances provided by the library
    /// You can add them to your types by deriving [`Patch`] with [`update_parser_with!`].
    fn check(&self, _errors: &mut Vec<String>) {}
}

/// Implement [`Patch`] for a structure that implements [`Parser`]
///
/// Can take multiple types at once. As with any traits you can only define them
/// for types declared in your crate.
/// ```rust
/// # use omnomnomicon::*;
/// #[derive(Debug, Clone, Parser)]
/// struct Foo(u32);
/// update_as_parser!(Foo);
/// ```
#[macro_export]
macro_rules! update_as_parser {
    ($($ty:ty),*) => {$(
        impl Patch for $ty {
            type Update = $ty;
            fn enter<'a>(&self, _: &'static str, input: &'a str)-> ::omnomnomicon::Result<'a, Self::Update> {
                ::omnomnomicon::with_hint(
                    || Some(::std::borrow::Cow::from(format!("cur: {:?}", self))),
                    ::omnomnomicon::Parser::parse,
                )(input)
            }
            fn apply(&mut self, update: Self::Update, _errors: &mut Vec<String>)  {
                *self = update;
            }
        }
    )*}
}
pub use update_as_parser;

update_as_parser!(u8, u16, u32, u64, i8, i16, i32, i64, usize, isize, f32, f64);

mod foo {
    use omnomnomicon::*;
    #[derive(Debug, Clone, Parser)]
    struct Foo(u32);
    update_parser_with! { Foo, self, update, errors, {
        if self.0 > update.0 {
            errors.push("value must increase".to_owned());
        }
    }}
}

/// A variant of [`update_as_parser`] that helps to define a custom check
///
/// As with any traits you can only define them for types declared in your crate
/// ```rust
/// # use omnomnomicon::{Parser, update_parser_with};
/// #[derive(Debug, Clone, Parser)]
/// struct Foo(u32);
/// update_parser_with! { Foo, self, update, errors, {
///     if self.0 > update.0 {
///         errors.push("value must increase".to_owned());
///     }
/// }}
/// ```
#[macro_export]
macro_rules! update_parser_with {
    ($ty:ty, $self:ident, $update:ident, $errors:ident, $body:expr) => {

        impl ::omnomnomicon::Patch for $ty
            where $ty: ::omnomnomicon::Parser,
{
            type Update = $ty;
            fn enter<'a>(&self, _: &'static str, input: &'a str)-> ::omnomnomicon::Result<'a, Self::Update> {
                ::omnomnomicon::with_hint(
                    || Some(::std::borrow::Cow::from(format!("cur: {:?}", self))),
                    ::omnomnomicon::Parser::parse,
                )(input)
            }
            fn apply(&mut $self, $update: Self::Update, $errors: &mut Vec<String>)  {{
                $body
                *$self = $update;
            }}
        }
    }
}

pub use update_parser_with;

#[test]
fn update_parser_with_works() {
    use crate::Parser;
    #[derive(Debug, Clone, Parser)]
    struct Foo(u32);
    update_parser_with! { Foo, self, update, errors, {
        if self.0 > update.0 {
            errors.push("value must increase".to_owned());
        }
    }}
}

impl<T: Parser + Clone + std::fmt::Debug> Patch for Option<T> {
    type Update = Option<T>;

    fn enter<'a>(&self, _: &'static str, input: &'a str) -> crate::Result<'a, Self::Update> {
        use crate::*;
        let enabled = tagged("some", fmap(Some, T::parse));
        let disabled = tag(None, "none");

        with_hint(
            || Some(std::borrow::Cow::from(format!("cur: {:?}", self))),
            or(enabled, disabled),
        )(input)
    }

    fn apply(&mut self, updater: Self::Update, _errors: &mut Vec<String>) {
        *self = updater;
    }
}

impl<T: Patch + std::fmt::Debug, const N: usize> Patch for [T; N] {
    type Update = (usize, T::Update);

    fn enter<'a>(&self, entry: &'static str, input: &'a str) -> crate::Result<'a, Self::Update> {
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

    fn apply(&mut self, updater: Self::Update, errors: &mut Vec<String>) {
        let before = errors.len();
        self[updater.0].apply(updater.1, errors);
        suffix_errors(before, errors, &format!("[{}]", updater.0));
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
/// Updater for for a map like structure
pub enum UpdateOrInsert<K, T> {
    /// Delete item at key `K`
    Del(K),
    /// Insert an item `T` to key `K`, this assumes previous item does not exist
    /// in case of vectors
    Ins(K, T),
    /// Update item at key `K` with updater `U`
    Update(K, T),
}

impl<T: Patch<Update = T> + Parser + std::fmt::Debug> Patch for Vec<T> {
    type Update = UpdateOrInsert<usize, T>;

    fn enter<'a>(&self, entry: &'static str, input: &'a str) -> Result<'a, Self::Update> {
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

    fn apply(&mut self, updater: Self::Update, errors: &mut Vec<String>) {
        let before = errors.len();
        let index;
        match updater {
            UpdateOrInsert::Del(ix) => {
                index = ix;
                if ix >= self.len() {
                    errors.push(format!("{} is not a valid index in 0..{}", ix, self.len()))
                } else {
                    self.remove(ix);
                }
            }
            UpdateOrInsert::Ins(ix, t) => {
                index = ix;
                if ix > self.len() {
                    errors.push(format!("{} is not a valid index in 0..{}", ix, self.len()))
                } else {
                    self.insert(ix, t);
                }
            }
            UpdateOrInsert::Update(ix, u) => {
                index = ix;
                if ix > self.len() {
                    errors.push(format!("{} is not a valid index in 0..{}", ix, self.len()))
                } else {
                    self[ix].apply(u, errors)
                }
            }
        }
        suffix_errors(before, errors, &format!("[{:?}]", index));
    }
}

#[cfg(feature = "enum-map")]
impl<K, V> Patch for enum_map::EnumMap<K, V>
where
    K: enum_map::EnumArray<V> + Copy + std::fmt::Debug,
    V: Patch,
{
    type Update = (K, <V as Patch>::Update);

    fn enter<'a>(&self, entry: &'static str, input: &'a str) -> Result<'a, Self::Update> {
        let table = self.iter().map(|(k, _)| (k, Cow::from(format!("{:?}", k))));
        let (output, _) = literal(entry)(input)?;
        let (output, key) = output.bind_space(true, lookup_key(table, 100))?;
        let (output, val) = output.bind_space(true, |i| self[key].enter(".", i))?;
        Ok((output, (key, val)))
    }

    fn apply(&mut self, (key, updater): Self::Update, errors: &mut Vec<String>) {
        let before = errors.len();
        self[key].apply(updater, errors);
        suffix_errors(before, errors, &format!("[{:?}]", key));
    }

    fn check(&self, _errors: &mut Vec<String>) {}
}
