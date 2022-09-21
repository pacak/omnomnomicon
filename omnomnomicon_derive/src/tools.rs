use syn::{parse::Parse, *};

pub fn camelize(s: &str) -> String {
    let mut res = String::with_capacity(s.len());
    let mut cap = true;
    for c in s.chars() {
        if c == '_' {
            cap = true;
        } else if cap {
            res.extend(c.to_uppercase());
            cap = false
        } else {
            res.push(c)
        }
    }
    res
}

#[cfg(test)]
mod tests {
    use super::camelize;
    #[track_caller]
    fn check_camel(input: &str, expected: &str) {
        assert_eq!(&camelize(input), expected)
    }
    #[test]
    fn camelize_test() {
        check_camel("foo", "Foo");
        check_camel("foo_bar", "FooBar");
        check_camel("foo_8bar", "Foo8bar");
        check_camel("foo_8_bar", "Foo8Bar");
        check_camel("foo__bar", "FooBar");
    }
}

pub struct Doc(pub String);
impl Parse for Doc {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        input.parse::<Token![=]>()?;
        let s = input.parse::<LitStr>()?.value();
        Ok(Doc(s.trim_start().to_string()))
    }
}

pub struct CheckAttr(pub Expr);
impl Parse for CheckAttr {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let input_copy = input.fork();
        let name = input.parse::<Ident>()?;
        if name == "check" {
            let content;
            let _ = parenthesized!(content in input);
            Ok(CheckAttr(content.parse()?))
        } else {
            Err(input_copy.error("Unknown keyword"))
        }
    }
}

pub enum Attr {
    Skip,
    Literal(String),
    Via(Ident),
    Check(Box<Expr>),
    Enter,
    Okay,
}
impl Parse for Attr {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let input_copy = input.fork();
        let name = input.parse::<Ident>()?;
        if name == "skip" {
            Ok(Attr::Skip)
        } else if name == "okay" {
            Ok(Attr::Okay)
        } else if name == "literal" {
            let content;
            let _ = parenthesized!(content in input);
            let lit = content.parse::<LitStr>()?.value();
            Ok(Attr::Literal(lit))
        } else if name == "via" {
            let content;
            let _ = parenthesized!(content in input);
            Ok(Attr::Via(content.parse()?))
        } else if name == "check" {
            let content;
            let _ = parenthesized!(content in input);
            Ok(Attr::Check(content.parse()?))
        } else if name == "enter" {
            Ok(Attr::Enter)
        } else {
            Err(input_copy.error("Unknown keyword"))
        }
    }
}
