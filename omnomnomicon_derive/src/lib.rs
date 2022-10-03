use syn::*;

mod parser;
mod patch;
mod tools;

use crate::parser::derive_parser_impl;
use crate::patch::Top;

#[proc_macro_derive(Updater, attributes(om))]
pub fn derive_macro(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    parse_macro_input!(input as Top).into()
}

#[proc_macro_derive(Parser, attributes(om))]
pub fn derive_parser(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match derive_parser_impl(parse_macro_input!(input as parser::ParseStruct)) {
        Ok(ok) => ok.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
