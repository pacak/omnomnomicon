use ::quote::{quote, ToTokens};
use syn::{
    braced, bracketed, parenthesized,
    parse::{Parse, ParseStream},
    parse2,
    punctuated::Punctuated,
    spanned::Spanned,
    token, Attribute, Expr, Ident, Type, Visibility,
};

use crate::tools::{camelize, Doc};

#[derive(Debug)]
struct Field {
    name: Option<Ident>,
    ty: Type,
    help: Option<String>,
    checks: Vec<Expr>,
    dchecks: Vec<Expr>,
    skip: bool,
}

impl Field {
    pub fn parse(input: ParseStream, named: bool, mut no_check: bool) -> syn::Result<Self> {
        let mut docs = Vec::new();
        let mut checks = Vec::new();
        let mut dchecks = Vec::new();
        let mut skip = false;
        for attr in input.call(Attribute::parse_outer)? {
            if attr.path.is_ident("doc") {
                let Doc(doc) = parse2(attr.tokens)?;
                docs.push(doc);
            } else if attr.path.is_ident("om") {
                attr.parse_args_with(|input: ParseStream| loop {
                    if input.is_empty() {
                        break Ok(());
                    }
                    let input_copy = input.fork();
                    let keyword = input.parse::<Ident>()?;
                    let content;
                    if keyword == "check" {
                        parenthesized!(content in input);
                        checks.push(content.parse::<Expr>()?);
                    } else if keyword == "dcheck" {
                        parenthesized!(content in input);
                        dchecks.push(content.parse::<Expr>()?);
                    } else if keyword == "no_check" {
                        no_check = true;
                    } else if keyword == "skip" {
                        skip = true;
                    } else {
                        return Err(input_copy.error("Unexpected attribute"));
                    }
                    if !input.is_empty() {
                        input.parse::<token::Comma>()?;
                    }
                })?;
            }
        }
        let help = if docs.is_empty() {
            None
        } else {
            Some(docs.join("\n"))
        };

        let input_copy = input.fork();
        let _vis = input.parse::<Visibility>()?;
        let name = if named {
            Some(input.parse::<Ident>()?)
        } else {
            None
        };
        input.parse::<token::Colon>()?;
        let ty = input.parse::<Type>()?;
        if !skip && !no_check && checks.is_empty() && dchecks.is_empty() {
            return Err(input_copy.error(
                "You need to specify at least one check, 'skip' or use 'no_check' attribute",
            ));
        }
        Ok(Field {
            name,
            ty,
            help,
            checks,
            dchecks,
            skip,
        })
    }

    pub fn variant(&self, ix: usize) -> Ident {
        match &self.name {
            Some(ident) => Ident::new(&camelize(&ident.to_string()), ident.span()),
            None => Ident::new(&format!("Field{}", ix), self.ty.span()),
        }
    }

    fn accessor(&self, ix: usize) -> proc_macro2::TokenStream {
        match &self.name {
            Some(name) => quote!(#name),
            None => quote!(#ix),
        }
    }

    fn ident(&self, ix: usize) -> String {
        match &self.name {
            Some(n) => n.to_string(),
            None => ix.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Top {
    ident: Ident,
    vis: Visibility,
    update: Ident,
    fields: Vec<Field>,
    checks: Vec<Expr>,
    dchecks: Vec<Expr>,
}

impl Parse for Top {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse::<Visibility>()?;
        if input.peek(token::Struct) {
            Ok(Self::parse_struct(vis, attrs, input)?)
        } else {
            Err(input.error("Only structs can be patched"))
        }
    }
}

impl Top {
    fn parse_struct(
        vis: Visibility,
        attrs: Vec<Attribute>,
        input: ParseStream,
    ) -> syn::Result<Self> {
        input.parse::<token::Struct>()?;
        let ident = input.parse::<Ident>()?;
        let update = Ident::new(&format!("{}Updater", ident), ident.span());
        let content;
        let fields: Punctuated<Field, token::Comma>;
        let mut no_check = false;
        let mut checks = Vec::new();
        let mut dchecks = Vec::new();
        for attr in attrs {
            if attr.path.is_ident("om") {
                attr.parse_args_with(|input: ParseStream| loop {
                    if input.is_empty() {
                        break Ok(());
                    }
                    let input_copy = input.fork();
                    let keyword = input.parse::<Ident>()?;
                    let content;
                    if keyword == "check" {
                        parenthesized!(content in input);
                        checks.push(content.parse::<Expr>()?);
                    } else if keyword == "dcheck" {
                        parenthesized!(content in input);
                        dchecks.push(content.parse::<Expr>()?);
                    } else if keyword == "no_check" {
                        no_check = true;
                    } else {
                        return Err(input_copy.error("Unexpected attribute"));
                    }

                    if !input.is_empty() {
                        input.parse::<token::Comma>()?;
                    }
                })?;
            }
        }

        if input.peek(token::Brace) {
            braced!(content in input);
            if no_check {
                fields = content.parse_terminated(|i| Field::parse(i, true, true))?;
            } else {
                fields = content.parse_terminated(|i| Field::parse(i, true, false))?;
            }
        } else if input.peek(token::Paren) {
            bracketed!(content in input);
            if no_check {
                fields = content.parse_terminated(|i| Field::parse(i, false, true))?;
            } else {
                fields = content.parse_terminated(|i| Field::parse(i, false, false))?;
            }
        } else {
            return Err(input.error("Expected struct in () or {}"));
        }
        let fields = fields.into_iter().filter(|f| !f.skip).collect::<Vec<_>>();
        Ok(Top {
            vis,
            ident,
            update,
            fields,
            checks,
            dchecks,
        })
    }
}

impl ToTokens for Top {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let Self {
            vis,
            ident,
            fields,
            update,
            checks,
            dchecks,
        } = self;

        let updater_fields = fields.iter().enumerate().map(|(ix, f @ Field { ty, .. })| {
            let variant = f.variant(ix);
            quote!(#variant(<#ty as ::omnomnomicon::Patch>::Update))
        });

        let outer_checks = checks.iter().map(|check| {
            quote! {
                if let Err(err) = (#check)(&self) {
                    errors.push(err);
                }
            }
        });

        let outer_dchecks = dchecks.iter().map(|check| {
            quote! {
                if let Err(err) = (#check)(&self, &update) {
                    errors.push(err);
                }
            }
        });

        let field_checks = fields.iter().enumerate().map(|(ix, f @ Field { ty, .. })| {
            let accessor = f.accessor(ix);
            let name = accessor.to_string();
            let checks = f.checks.iter().map(|check| {
                quote! {
                    if let Err(err) = (#check)(&self.#accessor) {
                        errors.push(err);
                    }
                }
            });
            quote!({
                let field_before = errors.len();
                #(#checks)*
                <#ty as ::omnomnomicon::Patch>::check(&self.#accessor, errors);
                ::omnomnomicon::suffix_errors(field_before, errors, #name);
            })
        });

        // attempts to enter field parsers, used in `enter`
        let field_attempts = fields.iter().enumerate().map(|(ix, f @ Field { ty, .. })| {
            let accessor = f.accessor(ix);
            let fname = f.ident(ix);
            let variant = f.variant(ix);

            let parser = quote! {
                let mut p = ::omnomnomicon::tagged(#fname, ::omnomnomicon::label_if_missing(#fname, |i| {
                    <#ty as ::omnomnomicon::Patch>::enter(&self.#accessor, ".", i)
                }));
            };

            let parser = match &f.help {
                Some(docs) => quote! {
                    #parser
                    let mut p = ::omnomnomicon::help(#docs, p);
                },
                None => parser,
            };

            quote! {
                #parser
                match p(input) {
                    Err(e) => err += e,
                    Ok((out, ok)) => return Ok((out, #update::#variant(ok))),
                };
            }
        });

        let field_matches = fields.iter().enumerate().map(|(ix, f @ Field { ty, .. })| {
            let accessor = f.accessor(ix);
            let variant = f.variant(ix);
            let name = accessor.to_string();
            let dchecks = f.dchecks.iter().map(|dcheck| {
                quote! {
                    if let Err(err) = (#dcheck)(&self.#accessor, &val) {
                        errors.push(err);
                    }
                }
            });
            quote! {
                #update::#variant(val) => {
                    let field_before = errors.len();
                    #(#dchecks)*
                    <#ty as ::omnomnomicon::Patch>::apply(&mut self.#accessor, val, errors);
                    ::omnomnomicon::suffix_errors(field_before, errors, #name);
                }
            }
        });

        let name = ident.to_string();
        quote! {
            #[derive(Debug, Clone)]
            #vis enum #update {
                #(#updater_fields,)*
            }
            impl ::omnomnomicon::Patch for #ident {
                type Update = #update;

                fn enter<'a>(
                    &self,
                    entry: &'static str,
                    input: &'a str,
                ) -> ::omnomnomicon::Result<'a, Self::Update> {
                    let input = ::omnomnomicon::literal(entry)(input)?.0.input;
                    let input = ::omnomnomicon::space(input)?.0.input;
                    let mut err = ::omnomnomicon::Terminate::default();

                    #(#field_attempts),*

                    Err(err)
                }

                fn check(&self, errors: &mut Vec<String>) {
                    let before = errors.len();
                    #(#outer_checks)*
                    #(#field_checks)*
                    ::omnomnomicon::suffix_errors(before, errors, #name);
                }

                fn apply(&mut self, update: Self::Update, errors: &mut Vec<String>) {
                    let before = errors.len();
                    #(#outer_dchecks)*
                    match update {
                        #(#field_matches)*
                    }
                    ::omnomnomicon::suffix_errors(before, errors, #name);
                }
            }
        }
        .to_tokens(tokens)
    }
}

impl From<Top> for proc_macro::TokenStream {
    fn from(top: Top) -> Self {
        top.to_token_stream().into()
    }
}

// -------------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::Top;
    use pretty_assertions::assert_eq;
    use quote::{quote, ToTokens};
    use syn::parse_quote;

    #[test]
    fn simple_struct() {
        let top: Top = parse_quote! {
            /// outer comment
            #[om(check(outer), dcheck(outerd))]
            pub(crate) struct Foo {
                /// inner comment
                #[om(check(sanity), dcheck(isanity))]
                pub field: u32,

                #[om(skip)]
                pub ignored: u32,
            }
        };

        let expected = quote! {
            #[derive(Debug, Clone)]
            pub(crate) enum FooUpdater {
                Field(<u32 as ::omnomnomicon::Patch>::Update),
            }

            impl ::omnomnomicon::Patch for Foo {
                type Update = FooUpdater;
                fn enter<'a>(
                    &self,
                    entry: &'static str,
                    input: &'a str,
                ) -> ::omnomnomicon::Result<'a, Self::Update> {
                    let input = ::omnomnomicon::literal(entry)(input)?.0.input;
                    let input = ::omnomnomicon::space(input)?.0.input;
                    let mut err = ::omnomnomicon::Terminate::default();
                    let mut p = ::omnomnomicon::tagged("field", ::omnomnomicon::label_if_missing("field", |i| {
                        <u32 as ::omnomnomicon::Patch>::enter(&self.field, ".", i)
                    }));
                    let mut p = ::omnomnomicon::help("inner comment", p);
                    match p(input) {
                        Err(e) => err += e,
                        Ok((out, ok)) => return Ok((out, FooUpdater::Field(ok))),
                    };
                    Err(err)
                }

                fn check(&self, errors: &mut Vec<String>) {
                    let before = errors.len();
                    if let Err(err) = (outer)(&self) {
                        errors.push(err);
                    }
                    {
                        let field_before = errors.len();
                        if let Err(err) = (sanity)(&self.field) {
                            errors.push(err);
                        }
                        <u32 as ::omnomnomicon::Patch>::check(&self.field, errors);
                        ::omnomnomicon::suffix_errors(field_before, errors, "field");
                    }
                    ::omnomnomicon::suffix_errors(before, errors, "Foo");
                }

                fn apply(&mut self, update: Self::Update, errors: &mut Vec<String>) {
                    let before = errors.len();
                    if let Err(err) = (outerd)(&self, &update) {
                        errors.push(err);
                    }
                    match update {
                        FooUpdater::Field(val) => {
                            let field_before = errors.len();
                            if let Err(err) = (isanity)(&self.field, &val) {
                                 errors.push(err);
                            }
                            <u32 as ::omnomnomicon::Patch>::apply(&mut self.field, val, errors);
                            ::omnomnomicon::suffix_errors(field_before, errors, "field");
                        }
                    }
                    ::omnomnomicon::suffix_errors(before, errors, "Foo");
                }
            }
        };

        assert_eq!(top.to_token_stream().to_string(), expected.to_string());
    }
}
