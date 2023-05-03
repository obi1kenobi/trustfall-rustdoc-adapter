// This crate exports:
// - `Foo` only as itself
// - `inner::Foo` only as itself, meaning that the glob import is a no-op here
// - `Bar` also as `inner::Bar`, so the glob import applies to it

pub struct Foo;

pub struct Bar;

pub mod inner {
    pub use super::*;

    // This implicitly overrides the re-export of `super::Foo`.
    //
    // Proof: note the compilation error in this playground link, which attempts to compare
    // the two `Foo` structs against each other:
    // https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=19793ff9edd1cf7b30edf7c7b0033408
    pub struct Foo;
}
