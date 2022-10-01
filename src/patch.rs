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
use crate::{with_hint, Parser, Result};
use std::borrow::Cow;

/// `Patch` is best though of associating a value `T` with type `U`, such that T.apply(U) gives `T`
/// with a new value.
///
/// For that `Patch` comes with an assocated type [`Update`](Patch::Update) and member [`check`](Patch::check)
///
/// Two remaining function help to parse the update from a string and to run the checks and collect
/// the results.
///
/// # Deriving `Patch`
/// You can derive `Patch` with a derive macro, it supports attributes prefixed with `#[om(xxx)]`
/// ```rust
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
/// ```rust
/// #[derive(Debug, Clone, Patch)]
/// #[om(check(XXX))] // <- check will get whole Foo structure as input
/// struct Foo {
///     #[om(check(xxxx))] // <- check will get
///     pub field1: u64,
///     #[om(check(xxxx))]
///     pub field2: u32
///     #[om(dcheck(xxxx))]
///     pub field3: u32
///     #[om(enter)]
///     pub bar: Bar,
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
    fn enter<'a>(&self, entry: &'static str, input: &'a str) -> Result<'a, Self::Update>;

    /// Apply the update, collect the errors into the vector.
    ///
    /// Note - when used diretly from the trait - value will be updated even with the errors
    /// You should clone before applying the change and decide after running both `apply` and
    /// `check`. `apply` only runs differential checksm not the sanity check
    fn apply(&mut self, update: Self::Update, errors: &mut Vec<String>);

    /// Run validation check, collect the results
    ///
    /// Does nothing for instances
    fn check(&self, _errors: &mut Vec<String>) {}
}

/// Implement [`Patch`] for a structure that implements [`Parser`]
macro_rules! update_as_parser {
    ($($ty:ty),*) => {$(
        impl Patch for $ty {
            type Update = $ty;
            fn enter<'a>(&self, _: &'static str, input: &'a str)-> Result<'a, Self::Update> {
                with_hint(
                    || Some(Cow::from(format!("cur: {:?}", self))),
                    Parser::parse,
                )(input)
            }
            fn apply(&mut self, update: Self::Update, _errors: &mut Vec<String>)  {
                *self = update;
            }
        }
    )*}
}

update_as_parser!(u8, u16, u32, u64, i8, i16, i32, i64, usize, isize, f32, f64);

/// A variant of [`update_as_parser`] that helps to define a custom check
macro_rules! update_parser_with {
    ($ty:ty, $self:ident, $update:ident, $errors:ident, $body:expr) => {

        impl Patch for $ty {
            type Update = $ty;
            fn enter<'a>(&self, _: &'static str, input: &'a str)-> Result<'a, Self::Update> {
                with_hint(
                    || Some(Cow::from(format!("cur: {:?}", self))),
                    Parser::parse,
                )(input)
            }
            fn apply(&mut $self, $update: Self::Update, $errors: &mut Vec<String>)  {{
                $body
            }}
        }
    }
}

#[derive(Debug, Clone, Parser)]
struct Foo(u32);
update_parser_with! { Foo, self, update, errors, {
    if self.0 > update.0 {
        errors.push("value must increase".to_owned());
    }
}}
