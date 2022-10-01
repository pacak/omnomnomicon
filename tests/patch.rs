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

#[test]
fn nested_struct() {
    #[derive(Debug, Clone, Patch, PartialEq)]
    #[om(no_check)] // <- check will get whole Foo structure as input
    struct Foo {
        field1: u64,
        bar: Bar,
    }

    #[derive(Debug, Clone, Patch, PartialEq)]
    #[om(no_check)]
    struct Bar {
        field: f64,
    }

    let mut item = Foo {
        field1: 12,
        bar: Bar { field: 3.15 },
    };

    let (o, patch) = item.enter("iv", "iv bar . field 330").unwrap();
    assert_eq!(o.input, "");
    let mut errors = Vec::new();
    item.apply(patch, &mut errors);
    assert_eq!(
        item,
        Foo {
            field1: 12,
            bar: Bar { field: 330.0 }
        }
    );
}
