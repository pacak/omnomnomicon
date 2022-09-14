use crate::tools::*;
use proc_macro2::{Span, TokenStream};
use quote::*;
use syn::{parse::Parse, punctuated::Punctuated, spanned::Spanned, *};

pub fn derive_updater_impl(omnom: OStruct) -> Result<TokenStream> {
    let OStruct {
        ident,
        updater,
        fields,
        ..
    } = omnom;

    let _crate = quote!(::omnomnomicon::prelude);
    let fields = fields.iter().filter(|f| !f.skip).collect::<Vec<_>>();

    if fields.is_empty() {
        return Err(Error::new(omnom.span, "No fields?"));
    }

    let updater_fields = fields.iter().map(|f| {
        let OField { variant, ty, .. } = &f;
        quote!(#variant(<#ty as ::omnomnomicon::Updater>::Updater))
    });
    let updater_decl = quote! {
        #[derive(Debug, Clone)]
        pub enum #updater {
            #(#updater_fields),*
        }
    };

    let field_matchers = fields.iter().map(|&f| {
        let OField { variant, ident, .. } = f;
        let fname = ident.to_string();
        let p = quote! {
            let mut p = #_crate::tagged(#fname, #_crate::label_if_missing(#fname, |i| self.#ident.enter(".", i)));
        };
        let p = match &f.docs {
            Some(docs) => quote! { #p; let mut p = #_crate::help(#docs, p);},
            None => p,
        };
        quote! { #p; let mut p = #_crate::fmap(#updater::#variant, p); }
    });

    let make_decl = quote! {
        fn enter<'a>(&self, entry: &'static str, input: &'a str) -> #_crate::Result<'a, Self::Updater> {
            let input =  #_crate::space(#_crate::literal(entry)(input)?.0.input)?.0.input;
            // TODO - use field search to create it
            let mut err = #_crate::Terminate::default();
            #( #field_matchers; match p(input) {
                Err(e) => err += e,
                ok => return ok,
            };)*
            return Err(err)
        }
    };

    let update_fields = fields.iter().map(|f| {
        let OField { variant, ident, .. } = &f;

        let mut upd = quote! { self.#ident.apply(f) };
        for check in f.checks.iter() {
            upd = quote! {
                #check(&self.#ident, &f)?;
                #upd
            };
        }

        quote! { #updater::#variant(f) => {
            #upd
        }}
    });

    let apply_decl = quote! {
        fn apply(&mut self, updater: Self::Updater) -> std::result::Result<(), String> {
            match updater {
                #(#update_fields),*
            }
        }
    };

    let r = quote! {
        #updater_decl

        impl #_crate::Updater for #ident{
            type Updater = #updater;
            #make_decl
            #apply_decl
        }
    };
    // println!("{}", &r);
    Ok(r)
}

#[derive(Debug)]
pub struct OStruct {
    span: Span,
    ident: Ident,
    updater: Ident,
    fields: Punctuated<OField, Token![,]>,
}

// /// docu
// pub foo: u32,
#[derive(Debug)]
struct OField {
    /// raw field name, `foo`
    ident: Ident,
    /// Variant name in Updater enum, `FooUpdater`
    variant: Ident,
    /// Field type, `u32`
    ty: Type,
    /// documentation string, if present, `"docu"`
    docs: Option<String>,
    /// whether to add a bound check, currently unused
    #[allow(dead_code)]
    bounded: bool,
    /// whether to skip
    skip: bool,
    checks: Vec<Expr>,
}

impl Parse for OStruct {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let _attrs = input.call(Attribute::parse_outer)?;
        let _vis = input.parse::<Visibility>()?;
        input.parse::<Token![struct]>()?;
        let ident = input.parse::<Ident>()?;
        let content;
        let updater = Ident::new(&format!("{}Updater", ident), ident.span());
        let _brace = braced!(content in input);
        Ok(OStruct {
            span: input.span(),
            ident,
            updater,
            fields: content.parse_terminated(OField::parse)?,
        })
    }
}

impl Parse for OField {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let mut docs = Vec::new();
        let mut skip = false;
        let mut bounded = false;
        let mut checks = Vec::new();
        let mut enter = false;
        for attr in input.call(Attribute::parse_outer)? {
            if attr.path.is_ident("doc") {
                let Doc(doc) = parse2(attr.tokens)?;
                docs.push(doc);
            } else if attr.path.is_ident("om") {
                for a in attr
                    .parse_args_with(Punctuated::<_, Token![,]>::parse_terminated)?
                    .into_iter()
                {
                    match a {
                        Attr::Skip => skip = true,
                        Attr::Bounded => bounded = true,
                        Attr::Check(upd) => checks.push(*upd),
                        Attr::Literal(_) | Attr::Via(_) => {
                            return Err(Error::new(attr.span(), "unexpected attribute"))
                        }
                        Attr::Enter => enter = true,
                    }
                }
            }
        }

        let field = Field::parse_named(input)?;

        if !(enter || !checks.is_empty() || skip) {
            return Err(Error::new(
                field.span(),
                "You need to specify either `enter` or `check` attribute",
            ));
        }

        let ident = match field.ident {
            Some(ident) => ident,
            None => return Err(Error::new(field.span(), "named field expected")),
        };

        let variant = Ident::new(&camelize(&ident.to_string()), ident.span());
        Ok(OField {
            docs: if docs.is_empty() {
                None
            } else {
                Some(docs.join("\n"))
            },
            ty: field.ty,
            bounded,
            ident,
            variant,
            skip,
            checks,
        })
    }
}
