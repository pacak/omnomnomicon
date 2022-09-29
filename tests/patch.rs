use omnomnomicon::*;

#[test]
fn simple_updater() {
    fn dbl(old: &u32, new: &u32, f: u32) -> std::result::Result<(), String> {
        if *new > *old * f {
            Err(format!(
                "new value can be at most double {} -> {}",
                old, new
            ))
        } else {
            Ok(())
        }
    }

    #[derive(Debug, Clone, Patch, Eq, PartialEq)]
    struct Foo {
        /// docs
        #[om(dcheck(|a, b| dbl(a, b, 2)))]
        field: u32,
    }

    let mut item = Foo { field: 10 };
    let (o, patch) = item.enter("iv", "iv field 50").unwrap();
    assert_eq!(o.input, "");
    let mut errors = Vec::new();
    item.apply(patch, &mut errors);
    assert_eq!(item, Foo { field: 50 });
    assert_eq!(
        errors[0],
        "new value can be at most double 10 -> 50, field, Foo"
    );
}
