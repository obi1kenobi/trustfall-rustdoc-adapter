#![no_std]

// This crate exports:
// - `Foo` only as itself.
// - `inner::First` (the function) as itself: the local definition shadows the glob import.
// - `Foo::Second` as `inner::Second` because of the glob import.

pub enum Foo {
    First(u8),
}

pub mod inner {
    pub use super::Foo::*;

    #[allow(non_snake_case)]
    pub fn First() {}
}
