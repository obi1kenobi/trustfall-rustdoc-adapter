// This crate exports:
// - `Foo` only as itself.
// - `inner::First` as itself: it's a function, variants are in the type namespace so no conflict.
// - `Foo::Second` as `inner::Second` because of the glob import.

pub enum Foo {
    First,
}

pub mod inner {
    pub use super::Foo::*;

    #[allow(non_snake_case)]
    pub fn First() {}
}
