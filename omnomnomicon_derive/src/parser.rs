use crate::tools::*;
use proc_macro2::TokenStream;
use quote::*;
use syn::{parse::Parse, punctuated::Punctuated, spanned::Spanned, *};

pub fn derive_parser_impl(omnom: ParseStruct) -> Result<TokenStream> {
    match omnom {
        ParseStruct::SimpleEnum(simple) => simple_enum_impl(simple),
        ParseStruct::SimpleStruct(simple) => simple_struct_impl(simple),
    }
}

fn simple_struct_impl(simple: SimpleStruct) -> Result<TokenStream> {
    let _crate = quote! {::omnomnomicon::prelude};
    let SimpleStruct {
        struct_ident, ty, ..
    } = simple;
    let p = quote! { <#ty as #_crate::Parser>::parse };
    let p = match simple.field_name {
        Some(var) => quote! { #_crate::fmap(|#var| #struct_ident { #var }, #p) },
        None => quote! { #_crate::fmap(#struct_ident, #p) },
    };
    let p = match simple.help {
        Some(help) => quote! { #_crate::help(#help, #p) },
        None => p,
    };
    let r = quote! {
        impl #_crate::Parser for #struct_ident {
            fn parse(input: &str) -> #_crate::Result<Self> {
                #p(input)
            }
        }
    };
    //println!("{:?}", r);
    Ok(r)
}

fn simple_enum_impl(simple: SimpleEnum) -> Result<TokenStream> {
    let _crate = quote!(::omnomnomicon::prelude);
    let enum_ident = simple.enum_ident;

    let field_matchers = simple.fields.into_iter().filter(|f| !f.skip).map(|f| {
        let SimpleEnumField {
            constr, literal, ..
        } = f;
        let p = match f.item {
            None => quote! { #_crate::constmap(#enum_ident::#constr, #_crate::literal(#literal)) },
            Some((ty, None)) => quote! {
                #_crate::fmap(#enum_ident::#constr,
                    #_crate::tagged(#literal, <#ty as #_crate::Parser>::parse))
            },
            Some((_ty, Some(p))) => quote! {
                #_crate::fmap(#enum_ident::#constr,
                    #_crate::tagged(#literal, #p))
            },
        };
        match f.help {
            Some(msg) => quote! { #_crate::help(#msg, #p) },
            None => p,
        }
    });

    let r = quote! {
        impl #_crate::Parser for #enum_ident {
            fn parse(input: &str) -> #_crate::Result<Self> {
                let mut err = #_crate::Terminate::default();

                #( match (#field_matchers)(input) {
                    Err(e) => err += e,
                    ok => return ok,
                };)*
                return Err(err)
            }
        }
    };

    // println!("{}", &r);
    Ok(r)
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub enum ParseStruct {
    SimpleEnum(SimpleEnum),
    SimpleStruct(SimpleStruct),
}

#[derive(Debug)]
pub struct SimpleEnum {
    /// enum type name
    enum_ident: Ident,
    fields: Punctuated<SimpleEnumField, Token![,]>,
}

#[derive(Debug)]
pub struct SimpleStruct {
    /// struct type name
    struct_ident: Ident,
    /// named or unnamed field,
    field_name: Option<Ident>,
    /// Field type
    ty: Type,
    /// help message, if any
    help: Option<String>,
}

#[derive(Debug)]
struct SimpleEnumField {
    /// enum variant constructor name
    constr: Ident,
    /// enum variant literal
    literal: String,
    /// help message, if any
    help: Option<String>,
    /// don't generate parser for this field,
    skip: bool,
    /// enum constructor field, if present with custom parser, if present
    item: Option<(Type, Option<Ident>)>,
}

impl Parse for ParseStruct {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let mut err = match SimpleEnum::parse(input) {
            Ok(simple) => return Ok(ParseStruct::SimpleEnum(simple)),
            Err(err) => err,
        };

        match SimpleStruct::parse(input) {
            Ok(simple) => return Ok(ParseStruct::SimpleStruct(simple)),
            Err(e) => err.combine(e),
        }
        Err(err)
    }
}

impl Parse for SimpleStruct {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let _vis = input.parse::<Visibility>()?;
        input.parse::<Token![struct]>()?;
        let mut docs = Vec::new();

        for attr in attrs.iter() {
            if attr.path.is_ident("doc") {
                let Doc(doc) = parse2(attr.tokens.clone())?;
                docs.push(doc);
            }
        }
        let struct_ident = input.parse()?;
        let lookahead = input.lookahead1();

        let content;
        let ty;
        let field_name;
        if lookahead.peek(token::Paren) {
            let _paren = parenthesized!(content in input);
            let _vis = content.parse::<Visibility>()?;
            ty = content.parse()?;
            field_name = None;
            input.parse::<Token![;]>()?;
        } else if lookahead.peek(token::Brace) {
            let _brace = braced!(content in input);
            let _vis = content.parse::<Visibility>()?;
            let ident = content.parse()?;
            field_name = Some(ident);
            let _colon = content.parse::<Token![:]>()?;
            ty = content.parse()?;
            content.parse::<Token![,]>()?;
        } else {
            return Err(lookahead.error());
        }

        Ok(SimpleStruct {
            struct_ident,
            field_name,
            ty,

            help: if docs.is_empty() {
                None
            } else {
                Some(docs.join("\n"))
            },
        })
    }
}

impl Parse for SimpleEnum {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let _attrs = input.call(Attribute::parse_outer)?;
        let _vis = input.parse::<Visibility>()?;
        input.parse::<Token![enum]>()?;
        let enum_ident = input.parse::<Ident>()?;
        let content;
        let _brace = braced!(content in input);
        Ok(SimpleEnum {
            enum_ident,
            fields: content.parse_terminated(SimpleEnumField::parse)?,
        })
    }
}

impl Parse for SimpleEnumField {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let variant = Variant::parse(input)?;

        let mut docs = Vec::new();
        let mut skip = false;
        let mut literal = None;
        let mut via = None;
        for attr in variant.attrs.iter() {
            if attr.path.is_ident("doc") {
                let Doc(doc) = parse2(attr.tokens.clone())?;
                docs.push(doc);
            } else if attr.path.is_ident("om") {
                for a in attr
                    .parse_args_with(Punctuated::<_, Token![,]>::parse_terminated)?
                    .into_iter()
                {
                    match a {
                        Attr::Skip => skip = true,
                        Attr::Literal(lit) => literal = Some(lit),
                        Attr::Via(x) => via = Some(x),
                        Attr::Check(_) => {}
                        Attr::Enter => {}
                        Attr::Okay => {}
                    }
                }
            }
        }

        let ty = match &variant.fields {
            _ if skip => None,
            Fields::Unnamed(xs) if xs.unnamed.len() == 1 => Some(xs.unnamed[0].ty.clone()),
            Fields::Unit => None,
            _ => return Err(Error::new(
                variant.fields.span(),
                "only unit fields are supported and fields with a single unnamed value are supported, you can #[om(skip)] it",
            )),

        };
        let item = match (ty, via) {
            (None, None) => None,
            (None, Some(_)) => {
                return Err(Error::new(
                    variant.fields.span(),
                    "via field makes no sense without a field with constructor",
                ))
            }
            (Some(ty), via) => Some((ty, via)),
        };

        Ok(SimpleEnumField {
            literal: literal.unwrap_or_else(|| variant.ident.to_string()),
            constr: variant.ident,
            skip,
            item,
            help: if docs.is_empty() {
                None
            } else {
                Some(docs.join("\n"))
            },
        })
    }
}
