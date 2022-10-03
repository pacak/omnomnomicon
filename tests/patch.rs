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

    #[derive(Debug, Clone, Updater, Eq, PartialEq)]
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
    #[derive(Debug, Clone, Updater, PartialEq)]
    #[om(no_check)] // <- check will get whole Foo structure as input
    struct Foo {
        field1: u64,
        bar: Bar,
    }

    #[derive(Debug, Clone, Updater, PartialEq)]
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

#[test]
fn test_element_checker() {
    fn sanity(val: &u32) -> std::result::Result<(), String> {
        if *val > 100 {
            Err(format!("{val} is too large, must be under 100"))
        } else {
            Ok(())
        }
    }

    fn isanity(val: &u32, upd: &u32) -> std::result::Result<(), String> {
        if *val > 1000 {
            Err(format!("{val} is too large, must be under 1000"))
        } else if *upd >= val * 2 {
            Err(format!("Increment is too large {upd} -> {val}"))
        } else {
            Ok(())
        }
    }

    fn ten_percent(val: u32) -> std::result::Result<(), String> {
        if val == 3 {
            Err("Shouldn't be 3".to_string())
        } else {
            Ok(())
        }
    }

    #[derive(Debug, Clone, Updater)]
    pub(crate) struct Foo {
        /// inner comment
        #[om(enter, check(sanity), enter, dcheck(isanity), enter, check(|cur| ten_percent(cur + 10)))]
        pub field: Vec<u32>,
    }

    let mut item = Foo {
        field: vec![1, 2, 3],
    };

    let (o, patch) = item.enter("iv", "iv field . 0 ins 99").unwrap();
    assert_eq!(o.input, "");
    let mut errors = Vec::new();
    item.apply(patch, &mut errors);

    assert!(errors.is_empty());
    assert_eq!([99, 1, 2, 3].as_slice(), &item.field);
}

#[test]
fn nested_with_parser_and_closure() {
    #[derive(Debug, Copy, Clone, Parser, Eq, PartialEq)]
    struct Price(u32);

    update_as_parser!(Price);
    fn ten_percent(a: u32, b: u32) -> std::result::Result<(), String> {
        if a + a / 10 > b {
            Err(format!(
                "increment should be at most 10%, it is now {a} -> {b}"
            ))
        } else {
            Ok(())
        }
    }

    #[derive(Debug, Clone, Updater)]
    pub(crate) struct Foo {
        /// inner comment
        #[om(enter, dcheck(|cur, upd| ten_percent(cur.0, upd.0)))]
        pub field: Vec<Price>,
    }

    let mut item = Foo {
        field: vec![Price(1000)],
    };

    let (o, patch) = item.enter("iv", "iv field . 0 upd 99").unwrap();
    assert_eq!(o.input, "");
    let mut errors = Vec::new();
    item.apply(patch, &mut errors);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0],
        "increment should be at most 10%, it is now 1000 -> 99, index 0, field, Foo"
    );
    assert_eq!([Price(99)].as_slice(), &item.field);
}
