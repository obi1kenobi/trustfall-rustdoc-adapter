#![no_std]

// This crate exports only the names `Bar` and `Baz`.
// While both `Foo` structs are public, their names conflict and are not exported.

mod a {
    pub struct Foo;

    pub struct Bar;
}
mod b {
    pub struct Foo;

    pub struct Baz;
}

#[expect(
    ambiguous_glob_reexports,
    reason = "test that conflicting glob re-exports make `Foo` unimportable"
)]
pub use a::*;
pub use b::*;
