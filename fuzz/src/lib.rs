use chrono::NaiveDateTime;
use enum_map::{enum_map, Enum, EnumMap};
use omnomnomicon::prelude::*;
use omnomnomicon::tutorial::{parse_command, Command};

#[derive(Debug, Clone)]
pub enum CommandExt {
    Tutorial(Command),
    Iv(<Config as Updater>::Updater),
    Date(NaiveDateTime),
    Action(Action),
}

pub fn parse_ext(input: &str) -> Result<CommandExt> {
    choice((
        fmap(CommandExt::Tutorial, parse_command),
        iv_cmd(&Config {
            price: 10,
            boosting: Some(123),
            target: 10,
            limits: Limits {
                high: 10,
                low: 10,
                mystery: (),
            },
            enum_map: enum_map! {
                Key::Bar => 10u32,
                Key::Baz => 10u32,
            },
            coefficients: [1, 2, 3, 4, 5],
        }),
        fmap(CommandExt::Date, tagged("date", NaiveDateTime::parse)),
        fmap(CommandExt::Action, tagged("action", Action::parse)),
    ))(input)
}

pub fn iv_cmd(config: &'_ Config) -> impl FnMut(&str) -> Result<CommandExt> + '_ {
    fmap(CommandExt::Iv, updater_for(config, "iv"))
}

impl Default for Limits {
    fn default() -> Self {
        Self {
            mystery: (),
            low: 10,
            high: 100,
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            price: 50,
            target: 150,
            limits: Limits::default(),
            boosting: None,
            coefficients: [10, 20, 30, 40, 50],
            enum_map: enum_map! {
                Key::Bar => 10u32,
                Key::Baz => 10u32,
            },
        }
    }
}

#[derive(Debug, Clone, Updater)]
pub struct Config {
    /// Price...
    pub price: u32,
    pub target: u32,
    pub limits: Limits,
    pub boosting: Option<u32>,
    /// A set of magical coefficients
    pub coefficients: [u32; 5],
    pub enum_map: EnumMap<Key, u32>,
}

#[derive(Updater, Debug, Clone)]
pub struct Limits {
    /// This field will be skipped from parser things
    #[om(skip)]
    pub mystery: (),

    /// Low limit for something important
    ///
    /// Software will try to keep a value of something important above that limit
    pub low: u32,

    /// High limit for something important
    ///
    /// Software will try to keep a value of something important below that limit
    pub high: u32,
}

#[derive(Debug, Clone, Copy, Parser)]
pub enum Action {
    /// Update the fitmeasure
    #[om(literal("fit_measure"))]
    Fitmeasure,
    /// Fit the stretch
    FitStretch,
    /// Fit the shape
    #[om(skip)]
    FitShape,
    /// Perform mystery operation
    Mystery(Mystery),
    /// Perform magical operation
    Magical(Magical),
}

#[derive(Debug, Enum, Copy, Clone)]
pub enum Key {
    Bar,
    Baz,
}

#[derive(Debug, Copy, Clone, Parser)]
/// Performs a mystery transformation, should be an odd number
pub struct Mystery(pub u32);

#[derive(Debug, Copy, Clone, Parser)]
/// Performs a mystery transformation, should be an even number
pub struct Magical {
    pub(crate) value: u32,
}
