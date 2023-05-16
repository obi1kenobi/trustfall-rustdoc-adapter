// This crate exports:
// - `Foo` only as itself
// - `inner::First` only as itself, overriding the glob import
// - `Foo::Second` as `inner::Second` because of the glob import

pub enum Foo {
    First,
    Second,
}

pub mod inner {
    pub use super::Foo::*;

    // This implicitly overrides the re-export of `super::Foo::First`,
    // because as a unit struct, `First` exports both a type and a value with that name.
    pub struct First;
}
