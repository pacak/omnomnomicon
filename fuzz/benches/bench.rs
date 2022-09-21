#![allow(clippy::blacklisted_name)]
use criterion::{black_box, criterion_group, criterion_main, Criterion};

use omnomnomicon::prelude::*;

fn bench_literal(c: &mut Criterion) {
    let input = "hello";
    c.bench_function("parse literal", |b| {
        b.iter(|| {
            let p = literal("hello");
            parse_result(p, black_box(input)).unwrap();
        })
    });
}

fn bench_number(c: &mut Criterion) {
    let input = "12345";
    c.bench_function("parse number", |b| {
        b.iter(|| {
            let p = number::<u32>;
            parse_result(p, black_box(input)).unwrap();
        })
    });
}

fn bench_pair(c: &mut Criterion) {
    let input = "hello world";
    c.bench_function("pair of literals", |b| {
        b.iter(|| {
            let p = pair(literal("hello "), literal("world"));
            parse_result(p, black_box(input)).unwrap();
        })
    });
}

fn bench_or(c: &mut Criterion) {
    let input = "world";
    c.bench_function("or of literals", |b| {
        b.iter(|| {
            let p = or(literal("hello"), literal("world"));
            parse_result(p, black_box(input)).unwrap();
        })
    });
}

fn bench_choice(c: &mut Criterion) {
    let input = "world";
    c.bench_function("choice of literals", |b| {
        b.iter(|| {
            let p = choice((literal("hello"), literal("world")));
            parse_result(p, black_box(input)).unwrap();
        })
    });
}

fn bench_number_label(c: &mut Criterion) {
    let input = "12345";
    c.bench_function("number with label", |b| {
        b.iter(|| {
            let p = label("my number", number::<u32>);
            parse_result(p, black_box(input)).unwrap();
        })
    });
}

criterion_group! {
    name = primitive;
    config = Criterion::default();
    targets =
        bench_literal,
        bench_number,
}

criterion_group! {
    name = combinators;
    config = Criterion::default();
    targets =
        bench_pair,
        bench_or,
        bench_choice,
}

criterion_group! {
    name = decorators;
    config = Criterion::default();
    targets =
        bench_number_label,
}

fn ok(_a: &u32, _b: &u32) -> std::result::Result<(), String> {
    Ok(())
}

fn bench_updater_struct(c: &mut Criterion) {
    #[derive(Updater, Copy, Clone, Debug)]
    struct Foo {
        #[om(check(ok))]
        bar: u32,
        #[om(check(ok))]
        baz: u32,
    }
    let input = "iv baz 50";
    let foo = Foo { bar: 10, baz: 10 };
    c.bench_function("updater via struct", |b| {
        b.iter(|| {
            let p = updater_for(&foo, "iv");
            parse_result(p, black_box(input)).unwrap();
        })
    });
}

fn bench_updater_enum(c: &mut Criterion) {
    use enum_map::{enum_map, Enum};
    use omnomnomicon::updater::*;
    #[derive(Debug, Enum, Copy, Clone)]
    enum Key {
        Bar,
        Baz,
    }
    let foo = enum_map! {
        Key::Bar => 10u32,
        Key::Baz => 10u32,
    };
    let input = "iv Baz 50";
    c.bench_function("updater via enum", |b| {
        b.iter(|| {
            let p = updater_for(&foo, "iv");
            parse_result(p, black_box(input)).unwrap();
        })
    });
}

criterion_group! {
    name = updater;
    config = Criterion::default();
    targets =
        bench_updater_struct,
        bench_updater_enum,
}

fn bench_typing(c: &mut Criterion) {
    let input = &[
        "pl",
        "place ",
        "place ask d",
        "place ask durian 312",
        "place ask durian 312 12345",
        "perm p",
        "perm p12",
        "perm p12 potato ",
        "perm p12 potato q12",
        "perm p12 potato q12 bid",
        "iv",
        "iv tar",
        "iv target 4",
        "dict tra",
        "dict transmogrification",
        "dict transform",
    ];

    c.bench_function("typing", |b| {
        b.iter(|| {
            for i in black_box(input) {
                let _r = omnomnomicon::tutorial::parse_command(i);
            }
        })
    });
}

criterion_group! {
    name = typing;
    config = Criterion::default();
    targets =
        bench_typing,
}

criterion_main!(primitive, combinators, decorators, updater, typing);
